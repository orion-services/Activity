package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.data.entity.Activity;
import dev.orion.data.entity.User;
import dev.orion.services.interfaces.ActivityService;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.util.UUID;

@ApplicationScoped
public class ActivityServiceImpl implements ActivityService {
    @Inject
    @RestClient
    UserClient userClient;

    @Override
    public UUID createActivity(String userToken) {
        return null;
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
