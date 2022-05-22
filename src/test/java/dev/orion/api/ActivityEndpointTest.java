package dev.orion.api;


import dev.orion.api.endpoint.ActivityEndpoint;
import dev.orion.api.endpoint.dto.CreateActivityRequestDtoV1;
import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.entity.User;
import dev.orion.commom.enums.UserStatus;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.common.http.TestHTTPEndpoint;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import net.bytebuddy.utility.RandomString;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import javax.transaction.Transactional;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.Optional;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.mockito.Mockito.when;

@QuarkusTest
@TestHTTPEndpoint(ActivityEndpoint.class)
@Disabled
public class ActivityEndpointTest {
    @InjectMock
    @RestClient
    UserClient userClient;

    final private String userExternalId = RandomString.make();
    final private String userName = RandomString.make();

    @BeforeEach
    public void setup() {
        UserClientResponse userClientResponse = new UserClientResponse(userExternalId, userName, true);
        when(userClient.getUserByExternalId(userExternalId))
                .thenReturn(userClientResponse);
    }

    @Test
    @DisplayName("It should create an activity when user is OK")
    void testActivityCreationOK() {
        var requestDto = new CreateActivityRequestDtoV1();
        requestDto.setUserExternalId(userExternalId);

        given()
                .contentType("application/json")
                .body(requestDto)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.CREATED.getStatusCode())
                .body("uuid", notNullValue());
    }

    @Test
    @DisplayName("It should validate the request body")
    void testActivityCreationValidation() {
        var requestDto = new CreateActivityRequestDtoV1();

        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(requestDto)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is("User may not be blank"));
    }

    @Test
    @DisplayName("It should throw a bad request when user is already in an activity")
    @Transactional
    void testActivityCreationUserError() {
        var requestDto = new CreateActivityRequestDtoV1();
        requestDto.setUserExternalId(userExternalId);
        var user = new User();
        user.status = UserStatus.CONNECTED;
        user.externalId = userExternalId;
        PanacheMock.mock(User.class);
        when(User.findUserByExternalId(userExternalId))
                .thenReturn(Optional.of(user));


        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(requestDto)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is(MessageFormat.format("User {0} is not available to join activity", userExternalId)));
    }

    @Test
    @DisplayName("It should throw a bad request when user is deactivated")
    void testActivityCreationUserDeactivate() {
        var requestDto = new CreateActivityRequestDtoV1();
        requestDto.setUserExternalId(userExternalId);
        UserClientResponse userClientResponse = new UserClientResponse(userExternalId, userName, false);
        when(userClient
                .getUserByExternalId(userExternalId))
                .thenReturn(userClientResponse);

        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(requestDto)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is(MessageFormat.format("User {0} is not available to join activity", userExternalId)));
    }
}
