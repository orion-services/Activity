package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.util.enums.UserStatus;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.util.UUID;

@ApplicationScoped
public class ActivityServiceImpl implements ActivityService {
    @Inject
    UserServiceImpl userService;

    @Override
    public UUID createActivity(String userExternalId) {
        User user = userService.getUserByExternalId(userExternalId);
        if ( !UserStatus.AVAILABLE.equals(user.status)) {
            throw new RuntimeException("User must be active");
        }

        Activity newActivity = new Activity();
        newActivity.userList.add(user);
        newActivity.isActive = true;

        Document newDocument = new Document();
        newDocument.content = "";
        newActivity.document = newDocument;

        newActivity.persist();

        return newActivity.uuid;
    }

    @Override
    public Activity addUserInActivity(UUID activityUuid, String userToken) {
        return null;
    }

    @Override
    public Activity removeUserFromActivity(UUID activityUuid, String userToken) {
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
}
