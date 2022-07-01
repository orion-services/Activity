package dev.orion.fixture;

import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import lombok.val;

import java.util.Set;
import java.util.UUID;

public class GroupFixture {
    public static GroupActivity createGroup(Activity activity, Document document, Set<User> participants) {
        val group = new GroupActivity();

        group.setActivityOwner(activity);
        group.addDocument(document);
        group.setCapacity(participants.size());
        group.setParticipants(participants);
        group.setUuid(UUID.randomUUID());

        return group;
    }

    public static Document generateDocument(Set<User> editors) {
        val document = new Document();

        document.setExternalId(UUID.randomUUID().toString());
        document.assignMultipleParticipants(editors);
        document.persist();

        return document;
    }
}
