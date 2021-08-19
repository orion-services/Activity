package dev.orion.services.interfaces;

import dev.orion.data.entity.Activity;
import dev.orion.services.dto.UserWithErrorDto;

import java.util.Set;
import java.util.UUID;

public interface ActivityService {
    UUID createActivity(String userToken);
    Activity addUserInActivity(UUID activityUuid, String userExternalId);
    Activity removeUserFromActivity(UUID activityUuid, String userExternalId);
    void disconnectUserFromActivity(UUID activityUuid, String userExternalId);
    Activity endActivity(UUID activityUuid);
    Boolean canUserEditDocument(UUID activityUuid, String userExternalId);
    void nextRound(UUID activityUuid);
    UserWithErrorDto inviteUsersToActivity(UUID activityUuid, Set<String> userList);
}
