package dev.orion;

import io.quarkus.test.junit.QuarkusTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;


@QuarkusTest
@Disabled
class DocumentOldResourceTest {

    @Test
    @DisplayName("it should let user edit document and send to document queue")
    void testDocumentEditionHappyRoad() {
        throw new RuntimeException("Implement this test");
    }

    @Test
    @DisplayName("it should NOT let user edit document WHEN it's NOT his turn")
    void testValidationOfUserNotInRightTurn() {
        throw new RuntimeException("Implement this test");
    }

    @Test
    @DisplayName("it should NOT let user edit document WHEN it's NOT registered in the activity")
    void testValidationOfUserNotInRegisteredActivity() {
        throw new RuntimeException("Implement this test");
    }

    @Test
    @DisplayName("it should NOT produce in document queue WHEN user can't edit it")
    void testDocumentNotProduceInQueueRuleError() {
        throw new RuntimeException("Implement this test");
    }
}
