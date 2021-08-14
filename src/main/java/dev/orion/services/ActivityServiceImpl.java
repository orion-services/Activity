package dev.orion.services;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.util.exceptions.UserInvalidOperationException;
import dev.orion.util.enums.UserStatus;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.Optional;
import java.util.UUID;

@ApplicationScoped
@Transactional
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserServiceImpl userService;

    @Override
    public UUID createActivity(String userExternalId) {
        UserCompleteDataDto completeUserData = userService.getCompleteUserData(userExternalId);
       validateUserToJoinActivity(completeUserData);

        Activity newActivity = new Activity();
        newActivity.createdBy = completeUserData.user;
        newActivity.isActive = true;

        Document newDocument = new Document();
        newDocument.content = "";
        newActivity.document = newDocument;

        newActivity.persist();

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
        activity.userList.add(completeUserData.user);
        completeUserData.user.status = UserStatus.CONNECTED;

        return activity;
    }

    @Override
    public Activity removeUserFromActivity(UUID activityUuid, String userExternalId) {
        throw new RuntimeException("Method not implemented yet");
    }

    @Override
    public Activity endActivity(UUID activityUuid) {
        throw new RuntimeException("Method not implemented yet");
    }

    @Override
    public Boolean canUserEditDocument(UUID activityUuid, User user) {
        throw new RuntimeException("Method not implemented yet");
    }

    private void validateUserToJoinActivity(UserCompleteDataDto user) {
        if ( !UserStatus.AVAILABLE.equals(user.status) || !user.isActive) {
            String exceptionMessage = MessageFormat.format("User {0} is not available to join activity", user.externalId);
            throw new UserInvalidOperationException(exceptionMessage);
        }
    }
}
