package dev.orion.services;

import dev.orion.broker.dto.ActivityUpdateMessageDto;
import dev.orion.broker.dto.DocumentUpdateDto;
import dev.orion.broker.producer.ActivityUpdateProducer;
import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.services.dto.ActivityExecutionDto;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
import dev.orion.util.AggregateException;
import io.quarkus.arc.log.LoggerName;
import lombok.AllArgsConstructor;
import lombok.val;
import org.jboss.logging.Logger;
import org.jboss.resteasy.spi.NotImplementedYetException;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

import static java.lang.Boolean.FALSE;

@ApplicationScoped
@Transactional
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserService userService;

    @Inject
    ActivityUpdateProducer activityUpdateProducer;

    @Inject
    DocumentUpdateProducer documentUpdateProducer;

    @Inject
    GroupService groupService;

    @Inject
    WorkflowManageService workflowManageService;

    @LoggerName("ActivityService")
    Logger logger;

    @Override
    public UUID createActivity(final String userExternalId, String workflowName) {
        val completeUserData = userService.getCompleteUserData(userExternalId);
        validateUserCanCreateActivity(completeUserData);

        val workflow = Workflow
                .findByName(workflowName)
                .orElseThrow(() -> new NotFoundException(MessageFormat.format("Workflow with name {0} not found", workflowName)));

        Activity newActivity = mountNewActivity(completeUserData, workflow);
        newActivity.persist();

        logger.info(MessageFormat.format("Activity created with UUID: {0} by user {1}", newActivity.uuid, userExternalId));
        return newActivity.uuid;
    }

    private Activity mountNewActivity(UserEnhancedWithExternalData completeUserData, Workflow workflow) {
        Activity activity = new Activity();
        activity.creator = completeUserData.userEntity;
        activity.isActive = true;
        activity.setWorkflow(workflow);

        return activity;
    }

    private void validateUserCanCreateActivity(UserEnhancedWithExternalData user) {
        Optional<UserInvalidOperationException> exceptionOptional = Optional.empty();
        if (!user.isActive) {
            val exceptionMessage = MessageFormat.format("The user {0} must be active to create an activity", user.uuid);
            exceptionOptional = Optional.of(new UserInvalidOperationException(exceptionMessage));
        }

        if (!user.role.contains(UserRoles.CREATOR)) {
            val exceptionMessage = MessageFormat.format("The user {0} must have role {1} to create an activity", user.uuid, UserRoles.CREATOR);
            exceptionOptional = Optional.of(new UserInvalidOperationException(exceptionMessage));
        }

        exceptionOptional.ifPresentOrElse(e -> {
            logger.error(e.getMessage());
            throw e;
        }, () -> logger.info(MessageFormat.format("User {0} is legit to create activity", user.uuid)));
    }

    @Override
    public Activity addUserInActivity(UUID activityUuid, String userExternalId) {
        UserEnhancedWithExternalData user = userService.getCompleteUserData(userExternalId);
        Optional<Activity> optionalActivity = Activity.findByIdOptional(activityUuid);
        setUserToAvailableIfDroppedFromActivity(user.userEntity);

        validateUserInsertion(optionalActivity, user, activityUuid);

        val activity = optionalActivity.get();
        activity.addParticipant(user.userEntity);

        logger.info(MessageFormat.format("User ({0}) added to activity: ({1})", user.uuid, activity.uuid));
        activity.persist();

        return activity;
    }

    private void validateUserInsertion(Optional<Activity> optionalActivity, UserEnhancedWithExternalData user, UUID activityUuid) {
        validateActivityToAddUser(optionalActivity, user, activityUuid);
        validateUserToJoinInNewActivity(user);
    }

    private void setUserToAvailableIfDroppedFromActivity(User user) {
        if (user.activity != null && !user.activity.isActive) {
            user.activity.remove(user);
        }
    }

    private void validateActivityToAddUser(Optional<Activity> optionalActivity, UserEnhancedWithExternalData user, UUID activityUuid) {
        if (optionalActivity.isEmpty()) {
            throw new UserInvalidOperationException(MessageFormat.format("Activity with UUID {0} not found", activityUuid));
        }

        val activity = optionalActivity.get();
        if (activity.actualStage != ActivityStage.PRE) {
            String exceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", user.uuid, activity.uuid);
            throw new UserInvalidOperationException(exceptionMessage);
        }

        if (FALSE == activity.isActive) {
            String exceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, user.uuid);
            throw new UserInvalidOperationException(exceptionMessage);
        }
    }

    private void validateUserToJoinInNewActivity(UserEnhancedWithExternalData user) throws UserInvalidOperationException {
        val validationFails = new LinkedList<String>();

        if (FALSE == Objects.isNull(user.getUserEntity().activity)) {
            String exceptionMessage = "it is already in another activity";
            validationFails.add(exceptionMessage);
        }

        if (FALSE == user.isActive) {
            String exceptionMessage = "it is not ACTIVE";
            validationFails.add(exceptionMessage);
        }

        if (!validationFails.isEmpty()) {
            String exceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: ", user.uuid);
            val exception = new UserInvalidOperationException(exceptionMessage, validationFails);

            logger.error(exception.getMessage());
            throw exception;
        }
    }

    @Override
    public Activity removeUserFromActivity(UUID activityUuid, String userExternalId) {
        throw new RuntimeException("Method not implemented yet");
    }

    @Override
    public void disconnectUserFromActivity(UUID activityUuid, String userExternalId) {
        throw new NotImplementedYetException("disconnectUserFromActivity is not implemented");
    }

    @Override
    public Activity endActivity(UUID activityUuid) {
        throw new NotImplementedYetException("endActivity");
    }

    @Override
    public Activity startActivity(UUID activityUUID) {
        val activity = (Activity) Activity.findByIdOptional(activityUUID).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("Activity {0} not found", activityUUID));
        });

        validateActivityToStart(activity);

        executeWorkflow(activity, activity.getCreator(), null);
        createGroupIfNotExists(activityUUID);

        activity.setActualStage(ActivityStage.DURING);
        activity.persist();
        return activity;
    }

    @Override
    public Activity execute(ActivityExecutionDto activityExecutionDto) throws NotFoundException, InvalidActivityActionException {
        val activityUUID = activityExecutionDto.getActivityUUID();
        val documentExternalId = activityExecutionDto.getDocumentExternalId();
        val userExternalId = activityExecutionDto.getUserExternalId();
        logger.infov("Start activity execution with user {0}, document {1} and activity: {2} to new content", userExternalId, documentExternalId, activityUUID);

        Activity activity = null;
        Document document = null;
        User participant = null;

        try {
            val entities = getEntitiesToExecute(activityUUID, documentExternalId, userExternalId);
            activity = entities.activity;
            document = entities.document;
            participant = entities.participant;

            validateExecution(activity, participant);
        } catch (NotFoundException e) {
            logger.error(e.getMessage());
            return null;
        } catch (InvalidActivityActionException e) {
            val errorBuilder = ActivityUpdateMessageDto.getErrorBuilder();
            val userError = errorBuilder
                    .externalUserId(userExternalId)
                    .code(3) // Code number is set just for example, there's no importance now. Must change
                    .message(e.getMessage()).build();
            if (Objects.nonNull(activity)) {
                sendActivityToProducer(activity, List.of(userError), UUID.randomUUID());
            }

            return null;
        }

        if (FALSE == executeWorkflow(activity, participant, document)) {
            return null;
        }


        try {
            val messageKey = UUID.randomUUID();
            sendDocumentToProducer(activityExecutionDto, messageKey);
            sendActivityToProducer(activity, List.of(), messageKey);
        } catch (RuntimeException exception) {
            documentQueueErrorHandler(exception, activity, userExternalId);
        }

        logger.infov("activity {0} successfully executed with user {1} and document {2} was execution.", activityUUID, userExternalId, documentExternalId);

        return activity;
    }

    private boolean executeWorkflow(Activity activity, User participant, Document document) {
        try {
            workflowManageService.apply(activity, participant, document);
            return true;
        } catch (AggregateException e) {
            val userErrors = new ArrayList<ActivityUpdateMessageDto.UserError>();
            e.getExceptions().forEach(runtimeException -> {
                val errorBuilder = ActivityUpdateMessageDto.getErrorBuilder();
                val userError = errorBuilder
                        .externalUserId(participant.externalId)
                        .code(3)
                        .message(runtimeException.getMessage())
                        .build();

                userErrors.add(userError);
            });
            sendActivityToProducer(activity, userErrors, UUID.randomUUID());
            return false;
        }

    }

    private void documentQueueErrorHandler(RuntimeException exception, Activity activity, String userExternalId) {
        val errorBuilder = ActivityUpdateMessageDto.getErrorBuilder();
        val activityUUID = activity.getUuid();

        activity.setIsActive(false);
        activity.persist();

        val exceptionMessage = MessageFormat.format("Document queue is out, setting activity {0} to inactivate until it comeback.", activityUUID);
        logger.errorv("Document queue is out, setting activity {0} to inactivate until it comeback.", activityUUID);

        errorBuilder
                .externalUserId(userExternalId)
                .code(3) // Code number is set just for example, there's no importance now. Must change
                .message(exceptionMessage);


        sendActivityToProducer(activity, List.of(errorBuilder.build()), UUID.randomUUID());
        throw exception;
    }

    private void sendDocumentToProducer(ActivityExecutionDto activityExecutionDto, UUID messageKey) {
        val activityUUID = activityExecutionDto.getActivityUUID();
        val documentExternalId = activityExecutionDto.getDocumentExternalId();
        val userExternalId = activityExecutionDto.getUserExternalId();
        val content = activityExecutionDto.getContent();

        try {
            val documentUpdateDto = new DocumentUpdateDto(documentExternalId, content, userExternalId, messageKey);
            documentUpdateProducer.sendMessage(documentUpdateDto);
        } catch (IOException e) {
            logger.errorv("Error when trying to send update of document ({0}) from activity {1} to queue. Exception: {2}", documentExternalId, activityUUID, e);
            e.printStackTrace();
            throw new RuntimeException("Error when trying to send update of document to Document queue");
        }
    }

    private void sendActivityToProducer(Activity activity, List<ActivityUpdateMessageDto.UserError> userErrors, UUID messageKey) {
        val activityUUID = activity.getUuid();

        try {
            val activityUpdateMessageDto = new ActivityUpdateMessageDto(activity, messageKey);
            userErrors.forEach(activityUpdateMessageDto::addError);
            activityUpdateProducer.sendMessage(activityUpdateMessageDto);
        } catch (IOException e) {
            logger.errorv("Error when trying to send update of activity {0} to queue. Exception: {1}",activityUUID, e);
            throw new RuntimeException("Error when trying to send update of activity to Activity queue");
        }
    }



    private void validateExecution(Activity activity, User participant) throws InvalidActivityActionException {
        validateActivityIsActive(activity);
        if (FALSE == activity.getParticipants().contains(participant)) {
            throw new InvalidActivityActionException(MessageFormat.format("User {0} is not in activity {1} ", participant.getExternalId(), activity.getUuid()));
        }
    }

    private ExecutionDto getEntitiesToExecute(UUID activityUUID, String documentExternalId, String userExternalId) throws NotFoundException {
        val activity = (Activity) Activity.findByIdOptional(activityUUID).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("Activity {0} not found", activityUUID));
        });

        val document = Document.findByExternalId(documentExternalId).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("Document {0} not found", documentExternalId));
        });

        val participant = User.findUserByExternalId(userExternalId).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("User {0} not found", userExternalId));
        });

        return new ExecutionDto(activity, document, participant);
    }

    private Set<User> getNotConnectedUsers(Activity activity) {
        return activity.participants.stream().filter(user -> user.status != UserStatus.CONNECTED).collect(Collectors.toSet());
    }

    private void validateActivityToStart(Activity activity) {
        validateActivityIsActive(activity);

        val notConnectedUsers = getNotConnectedUsers(activity);
        if (FALSE == notConnectedUsers.isEmpty()) {
            val notConnectedUsersUUID = notConnectedUsers.stream().map(User::getExternalId).collect(Collectors.toList());
            val exceptionMessage = MessageFormat.format("Activity {0} has the following users not connected: {1}", activity.uuid, notConnectedUsersUUID);
            throw new InvalidActivityActionException(exceptionMessage);
        }

        if (activity.getParticipants().isEmpty()) {
            val exceptionMessage = MessageFormat.format("Activity {0} has no participants to start", activity.getUuid());
            throw new InvalidActivityActionException(exceptionMessage);
        }
    }

    private void validateActivityIsActive(Activity activity) {
        if (FALSE == activity.isActive) {
            val exceptionMessage = MessageFormat.format("Activity {0} must be active", activity.uuid);
            throw new InvalidActivityActionException(exceptionMessage);
        }
    }

    private void createGroupIfNotExists(UUID activityUUID) {
        val activity = (Activity) Activity.findById(activityUUID);
        if (!activity.getGroupActivities().isEmpty()) {
            return;
        }

        groupService.createGroup(activity, activity.getParticipants());
    }

    @AllArgsConstructor
    private class ExecutionDto {
        Activity activity;
        Document document;
        User participant;
    }
}
