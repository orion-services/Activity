package dev.orion.api;

import dev.orion.api.endpoint.ActivityEndpoint;
import dev.orion.api.endpoint.UserEndpoint;
import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.entity.User;
import dev.orion.fixture.UserFixture;
import dev.orion.services.interfaces.UserService;
import io.quarkus.test.common.http.TestHTTPEndpoint;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mockito;

import javax.inject.Inject;
import java.util.UUID;

@QuarkusTest
@TestHTTPEndpoint(UserEndpoint.class)
public class UserApiTest {
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
}
