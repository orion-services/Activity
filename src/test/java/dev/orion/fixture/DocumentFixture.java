package dev.orion.fixture;

import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import lombok.val;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;

public class DocumentFixture {
    public static void mockFindByUserIdAndGroup(Activity usingActivity) {
        given(Document.findAllByUserIdAndGroup(anyString(), any(UUID.class))).will(invocation -> {
            val externalId = (String) invocation.getArgument(0);
            val groupId = (UUID) invocation.getArgument(1);

            val userGroup = usingActivity.getGroupActivities().stream().filter(groupActivity -> groupActivity.getUuid().equals(groupId)).findFirst().orElseThrow();
            val userDocument = userGroup.getDocuments().stream().filter(document -> {
                return document
                        .getParticipantsAssigned()
                        .stream()
                        .anyMatch(user -> user.getExternalId().equals(externalId));
            }).collect(Collectors.toList());

            return userDocument;
        });
    }

    public static Document createDocument(Set<User> editors) {
        val document = new Document();

        document.setExternalId(UUID.randomUUID().toString());
        document.assignMultipleParticipants(editors);

        return document;
    }
}
