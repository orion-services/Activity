package dev.orion.services.interfaces;

import dev.orion.commom.exception.InvalidActivityActionException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.services.dto.ActivityExecutionDto;

import javax.ws.rs.NotFoundException;
import java.util.UUID;

public interface ActivityService {
    UUID createActivity(String userToken, String workflowName);

    Activity addUserInActivity(UUID activityUuid, String userExternalId);

    Activity removeUserFromActivity(UUID activityUuid, String userExternalId);

    void disconnectUserFromActivity(UUID activityUuid, String userExternalId);

    Activity endActivity(Activity activity) throws NotFoundException, InvalidActivityActionException;

    Activity startActivity(UUID activityUUID);

    Activity execute(ActivityExecutionDto ActivityExecutionDto);
}
