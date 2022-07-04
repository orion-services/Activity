package dev.orion.api.endpoint.body;

import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.val;
import org.eclipse.microprofile.openapi.annotations.media.Schema;

import java.util.*;

@Setter
@Getter
@NoArgsConstructor
public class StartActivityResponseBody {
    @Schema(required = true)
    private final List<GroupDto> groups = new ArrayList<>();

    @Schema(required = true, example = "372bf2a5-0da3-47bd-8c94-4a09d25d362a")
    private UUID activityUUID;

    public StartActivityResponseBody(Activity activity) {
        val groupActivities = activity.getGroupActivities();
        groupActivities.forEach(this::addGroup);
        this.activityUUID = activity.getUuid();
    }

    public void addGroup(GroupActivity groupActivity) {
        val group = new GroupDto();
        group.Id = groupActivity.getUuid();
        groupActivity.getParticipants().stream().forEach(user -> group.addParticipant(user.externalId));
        groupActivity.getDocuments().stream().forEach(document -> group.addDocument(document));

        groups.add(group);
    }

    @Getter
    @NoArgsConstructor
    @Setter
    public static class GroupDto {
        @Schema(required = true)
        Set<String> participantId = new HashSet<>();

        @Schema(required = true)
        Set<Document> documents = new HashSet<>();

        @Schema(required = true)
        UUID Id;

        public void addParticipant(String externalId) {
            participantId.add(externalId);
        }

        public void addDocument(Document document) {
            documents.add(document);
        }
    }
}
