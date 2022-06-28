package dev.orion.entity;

import dev.orion.fixture.ActivityFixture;
import dev.orion.fixture.UserFixture;
import io.quarkus.test.junit.QuarkusTest;
import lombok.val;
import net.datafaker.Faker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import javax.transaction.Transactional;
import java.util.LinkedHashSet;
import java.util.UUID;

@QuarkusTest
@Transactional
public class DocumentTest {
    private UUID groupUUID;

    private String userExternalId;

    private Document originalDocument;

    @Test
    @DisplayName("[findByUserIdAndGroup] - Find document by user and group")
    public void testFindByUserIdAndGroup() {
        createDocument();

        val optionalDocument = Document.findAllByUserIdAndGroup(userExternalId, groupUUID);
        Assertions.assertEquals(1, optionalDocument.size());
    }

    @Test
    @DisplayName("[findByUserIdAndGroup] - Find multiple documents by user and group")
    public void testFindByUserIdAndGroupList() {
        createDocument();

        val secondDocument = new Document();
        secondDocument.setParticipantsAssigned(new LinkedHashSet<>(originalDocument.getParticipantsAssigned()));
        secondDocument.setGroupActivity(originalDocument.getGroupActivity());
        secondDocument.setGroupActivity(originalDocument.getGroupActivity());
        secondDocument.setExternalId(UUID.randomUUID().toString());
        secondDocument.persist();

        val originalDocument = Document.findAllByUserIdAndGroup(userExternalId, groupUUID);
        Assertions.assertEquals(2, originalDocument.size());
    }

    @Test
    @DisplayName("[findByUserIdAndGroup] - Find document by user and group should return empty")
    public void testFindByUserIdAndGroupGetEmpty() {
        createDocument();

        var optionalDocument = Document.findAllByUserIdAndGroup(userExternalId, UUID.randomUUID());
        Assertions.assertTrue(optionalDocument.isEmpty());
        optionalDocument = Document.findAllByUserIdAndGroup(Faker.instance().idNumber().toString(), groupUUID);
        Assertions.assertTrue(optionalDocument.isEmpty());
    }


    private void createDocument() {
        val user = UserFixture.generateUser();
        val user2 = UserFixture.generateUser();
        user.persist();
        user2.persist();
        val activity = ActivityFixture.generateActivity(user);
        activity.uuid = null;

        val groupActivity = new GroupActivity();
        groupActivity.setActivityOwner(activity);
        activity.persist();
        groupActivity.persist();

        originalDocument = new Document();
        val documentExternalId = UUID.randomUUID().toString();
        originalDocument.setExternalId(documentExternalId);
        originalDocument.setGroupActivity(groupActivity);

        originalDocument.addParticipant(user);
        originalDocument.addParticipant(user2);
        originalDocument.persist();
        groupUUID = groupActivity.getUuid();
        userExternalId = user.externalId;
    }
}
