package dev.orion.services.interfaces;

import dev.orion.entity.Activity;

import java.util.UUID;

public interface ActivityService {
    UUID createActivity(String userToken, String workflowName);
    Activity addUserInActivity(UUID activityUuid, String userExternalId);
    Activity removeUserFromActivity(UUID activityUuid, String userExternalId);
    void disconnectUserFromActivity(UUID activityUuid, String userExternalId);
    Activity endActivity(UUID activityUuid);

    Activity startActivity(UUID activityUUID);
    Boolean canUserEditDocument(UUID activityUuid, String userExternalId);
    void nextRound(UUID activityUuid);
}
