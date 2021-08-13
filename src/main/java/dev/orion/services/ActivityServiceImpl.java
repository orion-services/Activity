package dev.orion.services;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.util.enums.UserStatus;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.Optional;
import java.util.UUID;

@ApplicationScoped
@Transactional
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserServiceImpl userService;

    @Override
    public UUID createActivity(String userExternalId) {
        User user = userService.getUserByExternalId(userExternalId);
       validateUserAvailabilityToJoinActivity(user);

        Activity newActivity = new Activity();
        newActivity.createdBy = user;
        newActivity.isActive = true;

        Document newDocument = new Document();
        newDocument.content = "";
        newActivity.document = newDocument;

        newActivity.persist();

        return newActivity.uuid;
    }

    @Override
    public Activity addUserInActivity(UUID activityUuid, String userExternalId) {
        User user = userService.getUserByExternalId((userExternalId));
        Optional<Activity> activityOpt = Activity.findByIdOptional(activityUuid);
        validateUserAvailabilityToJoinActivity(user);
        if (activityOpt.isEmpty()) {
            //  @TODO Create proper exception
            throw new RuntimeException("Activity not found");
        }
        var activity = activityOpt.get();
        activity.userList.add(user);
        user.status = UserStatus.CONNECTED;

        return activity;
    }

    @Override
    public Activity removeUserFromActivity(UUID activityUuid, String userExternalId) {
        return null;
    }

    @Override
    public Activity endActivity(UUID activityUuid) {
        return null;
    }

    @Override
    public Boolean canUserEditDocument(UUID activityUuid, User user) {
        return null;
    }

    private void validateUserAvailabilityToJoinActivity(User user) {
        if ( !UserStatus.AVAILABLE.equals(user.status)) {
            //  @TODO Create proper exception
            throw new RuntimeException("User must be active");
        }
    }
}
