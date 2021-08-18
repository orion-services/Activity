package dev.orion.services;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.UserService;
import dev.orion.util.enums.UserStatus;
import dev.orion.util.exceptions.UserInvalidOperationException;
import io.quarkus.arc.log.LoggerName;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.*;

@ApplicationScoped
@Transactional
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserService userService;

    @LoggerName("ActivityService")
    Logger logger;

    @Override
    public UUID createActivity(String userExternalId) {
        UserCompleteDataDto completeUserData = userService.getCompleteUserData(userExternalId);
       validateUserToJoinActivity(completeUserData);

        Activity newActivity = new Activity();
        newActivity.createdBy = completeUserData.userEntity;
        newActivity.isActive = true;

        Document newDocument = new Document();
        newDocument.content = "";
        newActivity.document = newDocument;

        newActivity.persist();
        logger.info(MessageFormat.format("Activity created with UUID: {0} by user {1}", newActivity.uuid, userExternalId));
        return newActivity.uuid;
    }

    @Override
    public Activity addUserInActivity(UUID activityUuid, String userExternalId) {
        UserCompleteDataDto completeUserData = userService.getCompleteUserData(userExternalId);
        Optional<Activity> activityOpt = Activity.findByIdOptional(activityUuid);
        validateUserToJoinActivity(completeUserData);

        if (activityOpt.isEmpty()) {
            throw new UserInvalidOperationException("Activity not found");

        }
        var activity = activityOpt.get();
        activity.userList.add(completeUserData.userEntity);

        if (activity.userRound == null) {
            activity.userRound = completeUserData.userEntity;
        }
        completeUserData.userEntity.status = UserStatus.CONNECTED;

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

            return  activity;
        }

        return  null;
    }

    @Override
    public Boolean canUserEditDocument(UUID activityUuid, String userExternalId) {
        Optional<Activity> activityOptional = Activity.findByIdOptional(activityUuid);
        UserCompleteDataDto user = userService.getCompleteUserData(userExternalId);

        if (activityOptional.isEmpty() || user == null) {
            logger.warn(MessageFormat.format("Activity {0} not found", activityUuid));
            return false;
        }

        Activity activity = activityOptional.get();
        if (Boolean.TRUE.equals(user.isActive)
                && activity.userRound.equals(user.userEntity)
                && user.status.equals(UserStatus.CONNECTED)
                && Boolean.TRUE.equals(activity.isActive)) {
            return  true;
        }

        logger.info(MessageFormat.format("User {0} CANNOT edit activity {1}", userExternalId, activityUuid));
        return false;
    }

    @Override
    public void nextRound(UUID activityUuid) {
       Optional<Activity> activityOpt = Activity.findByIdOptional(activityUuid);
       if (activityOpt.isPresent()) {
           Activity activity = activityOpt.get();
           if (Boolean.FALSE.equals(activityHasOnlineParticipants(activity))) {
               this.endActivity(activity.uuid);
               throw new UserInvalidOperationException(MessageFormat.format("Activity {0} is deactivated due there is no online participants", activityUuid));
           }
           User userRound = activity.userRound;
           List<User> userList = activity.userList;
           User nextUserRound = userRound;

           int counter = 0;
           do {
               var indexOfUserRound = userList.indexOf(nextUserRound);
               if (indexOfUserRound == (userList.size() - 1)) {
                   nextUserRound = userList.get(0);
               } else {
                   nextUserRound = userList.get(++indexOfUserRound);
               }
               counter++;
           } while (nextUserRound.status != UserStatus.CONNECTED && counter < userList.size());

           logger.info(MessageFormat.format("Activity: {0} set the user {1} to next round", activity.uuid, nextUserRound.externalId));
           activity.userRound = nextUserRound;
       }
    }


    private void validateUserToJoinActivity(UserCompleteDataDto user) throws UserInvalidOperationException {
        if ( user.status != UserStatus.AVAILABLE || Boolean.FALSE.equals(user.isActive)) {
            String exceptionMessage = MessageFormat.format("User {0} is not available to join activity", user.uuid);
            throw new UserInvalidOperationException(exceptionMessage);
        }
    }

    private boolean activityHasOnlineParticipants(Activity activity) {
        return activity.userList.stream().anyMatch(user -> user.status == UserStatus.CONNECTED);
    }
}
