package dev.orion.services;

import dev.orion.broker.dto.ActivityUpdateMessageDto;
import dev.orion.broker.producer.ActivityUpdateProducer;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.User;
import dev.orion.entity.Workflow;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@ApplicationScoped
@Transactional
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserService userService;

    @Inject
    ActivityUpdateProducer activityUpdateProducer;

    @Inject
    GroupService groupService;

    @LoggerName("ActivityService")
    Logger logger;

    @Override
    public UUID createActivity(final String userExternalId, String workflowName) {
        val completeUserData = userService.getCompleteUserData(userExternalId);
        validateUserCanCreateActivity(completeUserData);

        val workflow = Workflow
                .findByName(workflowName)
                .orElseThrow(() -> new NotFoundException(MessageFormat.format("Workflow with name {} not found", workflowName)));

        Activity newActivity = new Activity();
        newActivity.createdBy = completeUserData.userEntity;
        newActivity.isActive = true;
        newActivity.setWorkflow(workflow);
        newActivity.addGroup(groupService.createGroup(newActivity));

        newActivity.persist();
        logger.info(MessageFormat.format("Activity created with UUID: {0} by user {1}", newActivity.uuid, userExternalId));
        return newActivity.uuid;
    }

    private void validateUserCanCreateActivity(UserEnhancedWithExternalData user) {
        if (!user.isActive) {
            val exceptionMessage = MessageFormat.format("The user {0} must be active to create an activity", user.uuid);
            throw new UserInvalidOperationException(exceptionMessage);
        }

//        if (user.role)
    }

    @Override
    public Activity addUserInActivity(UUID activityUuid, String userExternalId) {
        UserEnhancedWithExternalData user = userService.getCompleteUserData(userExternalId);
        Optional<Activity> activityOpt = Activity.findByIdOptional(activityUuid);

        if (activityOpt.isEmpty()) {
            throw new UserInvalidOperationException("Activity not found");
        }

        var activity = activityOpt.get();
        if (activity.userList.contains(user.userEntity)) {
            if (user.isActive == Boolean.FALSE) {
                throw new UserInvalidOperationException(MessageFormat.format("User {0} must be active to join in activity", userExternalId));
            } else if (activity.isActive == Boolean.FALSE) {
                throw new UserInvalidOperationException(MessageFormat.format("Activity {0} must be active to add an user", activity.uuid));
            }
        } else {
            if (user.userEntity.activity != null && user.userEntity.status == UserStatus.DISCONNECTED) {
                user.userEntity.activity.userList.remove(user.userEntity);
                user.userEntity.status = UserStatus.AVAILABLE;
            }
            validateUserToJoinInNewActivity(user);
            activity.userList.add(user.userEntity);
        }


        if (activity.userRound == null) {
            activity.userRound = user.userEntity;
        }
        user.userEntity.status = UserStatus.CONNECTED;
        user.userEntity.activity = activity;

        logger.info(MessageFormat.format("User ({0}) added to activity: ({1})", user.uuid, activity.uuid));

        return activity;
    }

    @Override
    public Activity removeUserFromActivity(UUID activityUuid, String userExternalId) {
        throw new RuntimeException("Method not implemented yet");
    }

    @Override
    public void disconnectUserFromActivity(UUID activityUuid, String userExternalId) {
        Optional<Activity> activityOptional = Activity
                .findByIdOptional(activityUuid);
        if (activityOptional.isPresent()) {
            Optional<User> disconnectedUser = activityOptional
                    .get()
                    .userList
                    .stream()
                    .filter(user -> user.externalId.equals(userExternalId))
                    .findFirst();

            disconnectedUser.ifPresent(user -> {
                user.status = UserStatus.DISCONNECTED;
                Activity activity = activityOptional.get();
                logger.info(MessageFormat.format("User {0} was disconnect from activity {1}", userExternalId, activityUuid));
                if (activity.userRound.equals(user) && this.activityHasOnlineParticipants(activity)) {
                    this.nextRound(activityUuid);
                } else {
                    this.endActivity(activityUuid);
                    logger.info(MessageFormat.format("Activity {0} is deactivated due there is no online participants", activityUuid));
                }
            });
        }

    }

    @Override
    public Activity endActivity(UUID activityUuid) {
        Optional<Activity> activityOptional = Activity.findByIdOptional(activityUuid);
        if (activityOptional.isPresent()) {
            var activity = activityOptional.get();
            activity.isActive = false;
//            Make all users available to get in another activity
            activity.userList.forEach(user -> user.status = UserStatus.AVAILABLE);

            return activity;
        }

        return null;
    }

    @Override
    public Boolean canUserEditDocument(UUID activityUuid, String userExternalId) {
        Optional<Activity> activityOptional = Activity.findByIdOptional(activityUuid);
        UserEnhancedWithExternalData user = userService.getCompleteUserData(userExternalId);

        if (activityOptional.isEmpty() || user == null) {
            logger.warn(MessageFormat.format("Activity {0} not found", activityUuid));
            return false;
        }

        Activity activity = activityOptional.get();
        if (Boolean.TRUE.equals(user.isActive)
                && activity.userRound.equals(user.userEntity)
                && user.status.equals(UserStatus.CONNECTED)
                && Boolean.TRUE.equals(activity.isActive)) {
            return true;
        }

        logger.info(MessageFormat.format("User {0} CANNOT edit activity {1}", userExternalId, activityUuid));

        ActivityUpdateMessageDto activityUpdateMessageDto = new ActivityUpdateMessageDto(activity);
        var errorType = activityUpdateMessageDto.getErrorType();
        var userError = activityUpdateMessageDto.getUserError();

        errorType.code = 1;
        errorType.message = MessageFormat.format("User {0} CANNOT edit activity", userExternalId);
        userError.externalUserId = userExternalId;
        userError.type = errorType;
        activityUpdateMessageDto.addError(userError);


        try {
            activityUpdateProducer.sendMessage(activityUpdateMessageDto);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    @Override
    public void nextRound(UUID activityUuid) throws UserInvalidOperationException {
        Optional<Activity> activityOpt = Activity.findByIdOptional(activityUuid);
        if (activityOpt.isPresent()) {
            Activity activity = activityOpt.get();
            if (Boolean.FALSE.equals(activityHasOnlineParticipants(activity))) {
                this.endActivity(activity.uuid);
                throw new UserInvalidOperationException(MessageFormat.format("Activity {0} is deactivated due there is no online participants", activityUuid));
            }
            User nextUserRound = getNextUserRound(activity);

            logger.info(MessageFormat.format("Activity: {0} set the user {1} to next round", activity.uuid, nextUserRound.externalId));

            activity.userRound = nextUserRound;
            ActivityUpdateMessageDto activityUpdateMessageDto = new ActivityUpdateMessageDto(activity);
            try {
                activityUpdateProducer.sendMessage(activityUpdateMessageDto);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


    private void validateUserToJoinInNewActivity(UserEnhancedWithExternalData user) throws UserInvalidOperationException {
        if (user.status != UserStatus.AVAILABLE || Boolean.FALSE.equals(user.isActive)) {
            String exceptionMessage = MessageFormat.format("User {0} is not available to join activity", user.uuid);
            throw new UserInvalidOperationException(exceptionMessage);
        }
    }

    private boolean activityHasOnlineParticipants(Activity activity) {
        return activity.userList.stream().anyMatch(user -> user.status == UserStatus.CONNECTED);
    }

    private User getNextUserRound(Activity activity) {
        User userRound = activity.userRound;
        List<User> userQueue = new ArrayList<>(activity.userList);

        int userIndexOnQueue = userQueue.indexOf(userRound);

        if (userIndexOnQueue == activity.userList.size()) {
            return userQueue.get(0);
        }

        return activity
                .userList
                .stream()
                .skip(userIndexOnQueue)
                .findFirst()
                .get();
    }
}
