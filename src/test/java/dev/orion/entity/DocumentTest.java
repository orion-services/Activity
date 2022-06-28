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
import java.util.UUID;

@QuarkusTest
@Transactional
public class DocumentTest {
    private UUID groupUUID;

    private String userExternalId;

    @Test
    @DisplayName("[findByUserIdAndGroup] - Find document by user and group")
    public void testFindByUserIdAndGroup() {
        createDocument();

        val optionalDocument = Document.findByUserIdAndGroup(userExternalId, groupUUID);
        Assertions.assertFalse(optionalDocument.isEmpty());
    }

    @Test
    @DisplayName("[findByUserIdAndGroup] - Find document by user and group should return empty")
    public void testFindByUserIdAndGroupGetEmpty() {
        createDocument();

        var optionalDocument = Document.findByUserIdAndGroup(userExternalId, UUID.randomUUID());
        Assertions.assertTrue(optionalDocument.isEmpty());
        optionalDocument = Document.findByUserIdAndGroup(Faker.instance().idNumber().toString(), groupUUID);
        Assertions.assertTrue(optionalDocument.isEmpty());
    }


    private void createDocument() {
        val user = UserFixture.generateUser();
        user.persist();
        val activity = ActivityFixture.generateActivity(user);
        activity.uuid = null;

        val groupActivity = new GroupActivity();
        groupActivity.setActivityOwner(activity);
        activity.persist();
        groupActivity.persist();

        val document = new Document();
        val documentExternalId = UUID.randomUUID().toString();
        document.setExternalId(documentExternalId);
        document.setGroupActivity(groupActivity);

        document.addParticipant(user);
        document.persist();
        groupUUID = groupActivity.getUuid();
        userExternalId = user.externalId;
    }
}
