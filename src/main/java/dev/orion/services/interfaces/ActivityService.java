package dev.orion.services.interfaces;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.User;

import java.util.UUID;

public interface ActivityService {
    public UUID createActivity(String userToken);
    public Activity addUserInActivity(UUID activityUuid, String userExternalId);
    public Activity removeUserFromActivity(UUID activityUuid, String userExternalId);
    public Activity endActivity(UUID activityUuid);
    public Boolean canUserEditDocument(UUID activityUuid, User user);
}
