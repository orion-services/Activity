package dev.orion.api.endpoint.body;

import dev.orion.entity.Activity;
import dev.orion.entity.GroupActivity;
import lombok.*;
import org.eclipse.microprofile.openapi.annotations.media.Schema;

import java.util.*;

@Setter
@Getter
@NoArgsConstructor
public class StartActivityResponseBody {
    @Schema(required = true, example = "{70f4bb61-4c6e-4aac-bb1d-56e4247e16b3: [participantId: be638c24-34de-4771-8e18-7af05ab8962d]}")
    private final Map<UUID, Participants> groups = new HashMap<>();

    @Schema(required = true, example = "372bf2a5-0da3-47bd-8c94-4a09d25d362a")
    private UUID activityUUID;

    public StartActivityResponseBody(Activity activity) {
        val groupActivities = activity.getGroupActivities();
        groupActivities.forEach(this::addGroup);
        this.activityUUID = activity.getUuid();
    }

    public void addGroup(GroupActivity groupActivity) {
        val participants = new Participants();
        groupActivity.getParticipants().stream().forEach(user -> participants.add(user.externalId));
        groups.put(groupActivity.getUuid(), participants);
    }

    @Getter
    @NoArgsConstructor
    @Setter
    public static class Participants {
        Set<String> participantId = new HashSet<>();

        public void add(String externalId) {
            participantId.add(externalId);
        }
    }
}
