package dev.orion.services;

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
import io.quarkus.arc.log.LoggerName;
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

        workflowManageService.apply(activity, activity.getCreator(), null);
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
        val content = activityExecutionDto.getContent();
        logger.infov("Start activity execution with user {0}, document {1} and activity: {2} to new content", userExternalId, documentExternalId, activityUUID);

        val activity = (Activity) Activity.findByIdOptional(activityUUID).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("Activity {0} not found", activityUUID));
        });

        val document = Document.findByExternalId(documentExternalId).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("Document {0} not found", documentExternalId));
        });

        val participant = User.findUserByExternalId(userExternalId).orElseThrow(() -> {
            throw new NotFoundException(MessageFormat.format("User {0} not found", userExternalId));
        });

        validateActivityIsActive(activity);
        if (FALSE == activity.getUserList().contains(participant)) {
            throw new InvalidActivityActionException(MessageFormat.format("User {0} is not in activity {1} ", userExternalId, activityUUID));
        }

        workflowManageService.apply(activity, participant, document);

        try {
            val documentUpdateDto = new DocumentUpdateDto(UUID.fromString(documentExternalId), content, userExternalId);
            documentUpdateProducer.sendMessage(documentUpdateDto);
        } catch (IOException e) {
            logger.errorv("Error when trying to send update of document ({0}) from activity {1} to document queue. Exception: {2}", documentExternalId, activityUUID, e);
            throw new RuntimeException("Error when trying to send update of document to document queue");
        }

        return activity;
    }



    private Set<User> getNotConnectedUsers(Activity activity) {
        return activity.userList.stream().filter(user -> user.status != UserStatus.CONNECTED).collect(Collectors.toSet());
    }

    private void validateActivityToStart(Activity activity) {
        validateActivityIsActive(activity);

        val notConnectedUsers = getNotConnectedUsers(activity);
        if (FALSE == notConnectedUsers.isEmpty()) {
            val notConnectedUsersUUID = notConnectedUsers.stream().map(User::getExternalId).collect(Collectors.toList());
            val exceptionMessage = MessageFormat.format("Activity {0} has the following users not connected: {1}", activity.uuid, notConnectedUsersUUID);
            throw new InvalidActivityActionException(exceptionMessage);
        }

        if (activity.getUserList().isEmpty()) {
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

        groupService.createGroup(activity, activity.getUserList());
    }
}
