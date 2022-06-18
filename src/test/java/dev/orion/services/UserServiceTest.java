package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.User;
import dev.orion.fixture.UserFixture;
import dev.orion.services.interfaces.UserService;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.*;
import org.mockito.BDDMockito;
import org.mockito.Mockito;

import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.BDDMockito.*;

@QuarkusTest
public class UserServiceTest {
    @Inject
    UserService testThis;

    @InjectMock
    @RestClient
    UserClient userClient;

    @InjectMock
    Session session;

    private final String commonUserExternalId = UUID.randomUUID().toString();
    private UserClientResponse userClientResponse;
    private User user;

    @BeforeEach
    public void setup() {
        Mockito.doNothing().when(session).persist(Mockito.any());
        userClientResponse = UserFixture.mockUserClient(userClient, commonUserExternalId);
        user = UserFixture.mockFindUserByExternalId(commonUserExternalId);
    }

    @Test
    @DisplayName("[connectUser] Should set user to connected")
    public void testUserConnection() {
        testThis.connectUser(commonUserExternalId);

        then(session).should(times(1)).persist(any());
        Assertions.assertEquals(UserStatus.CONNECTED, user.getStatus());
    }

    @Test
    @DisplayName("[connectUser] Should not set user to connected if it's not in activity")
    public void testUserConnectionIsInActivity() {
        user.activity = null;
        val exceptionMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            testThis.connectUser(commonUserExternalId);
        }).getMessage();

        val expectedMessage = MessageFormat.format("The user {0} must be in activity to be connected", commonUserExternalId);
        Assertions.assertEquals(expectedMessage, exceptionMessage);
        then(session).should(never()).persist(any());
    }

}
