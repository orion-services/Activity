package dev.orion.api.endpoint.body;

import dev.orion.entity.Activity;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.val;

import java.util.*;
import java.util.stream.Collectors;

@Setter
@Getter
@NoArgsConstructor
public class StartActivityResponseBody {
    private final Map<UUID, List<String>> groups = new HashMap<>();
    private UUID activityUUID;

    public StartActivityResponseBody(Activity activity) {
        val groupActivities = activity.getGroupActivities();
        groupActivities.forEach(this::addGroup);
        this.activityUUID = activity.getUuid();
    }

    public void addGroup(GroupActivity groupActivity) {
        val userIdList = groupActivity.getParticipants().stream().map(User::getExternalId).collect(Collectors.toList());
        groups.put(groupActivity.getUuid(), userIdList);
    }
}
