package dev.orion.api;


import dev.orion.api.endpoint.ActivityEndpoint;
import dev.orion.api.endpoint.dto.AddUserToActivityRequestDtoV1;
import dev.orion.api.endpoint.dto.CreateActivityRequestDtoV1;
import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.UserRoles;
import dev.orion.entity.Activity;
import dev.orion.fixture.UserFixture;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.util.setup.WorkflowStarter;
import io.quarkus.test.common.http.TestHTTPEndpoint;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import net.bytebuddy.utility.RandomString;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.UUID;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

@QuarkusTest
@TestHTTPEndpoint(ActivityEndpoint.class)
@Disabled
public class ActivityEndpointTest {
    final private String userExternalId = RandomString.make();
    final private String userName = Faker.instance().name().fullName();
    @InjectMock
    @RestClient
    UserClient userClient;
    @InjectSpy
    ActivityService activityService;
    @InjectSpy
    GroupService groupService;

    UUID groupUuid;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this); // Start mocks

        UserClientResponse userClientResponse = UserFixture.generateClientResponseDto();
        userClientResponse.uuid = userExternalId;
        userClientResponse.name = userName;

        when(userClient.getUserByExternalId(userExternalId))
                .thenReturn(userClientResponse);
//        generateActivityAndGroup();
    }
    private void generateActivityAndGroup() {
        val activityUuid = activityService.createActivity(userExternalId, WorkflowStarter.GENERIC_WORKFLOW_NAME);

        val activity = (Activity) Activity.findById(activityUuid);
        groupUuid = groupService.createGroup(activity).getUuid();

        Mockito.clearInvocations(activityService, groupService);
    }

    //    Activity creation
    @Test
    @DisplayName("[/ - POST] API must create Activity in good scenario")
    void testActivityCreationOK() {
        val uuid = (String) given()
                .contentType("application/json")
                .body(getActivityCreationRequestDto())
                .when()
                .post()
                .then()
                .statusCode(Response.Status.CREATED.getStatusCode())
                .body("uuid", notNullValue(), "groups", hasItem(notNullValue()))
                .extract()
                .path("uuid");

        then(activityService).should().createActivity(userExternalId, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val activity = (Activity) Activity.findById(UUID.fromString(uuid));
        then(groupService).should().createGroup(activity);
    }

    @Test
    @DisplayName("[/ - POST] It should validate the request body")
    void testActivityCreationValidation() {
        val requestBody = getActivityCreationRequestDto();
        requestBody.setUserExternalId(null);
        requestBody.setWorkflowName(null);

        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(requestBody)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors", hasItems("Workflow may not be blank", "User may not be blank"));

        then(activityService).should(never()).createActivity(anyString(), anyString());
        then(groupService).should(never()).createGroup(any());
    }

    @Test
    @DisplayName("[/ - POST] It should throw a bad request when user is deactivated")
    void testActivityCreationUserDeactivate() {
        val userClientResponse = UserFixture.generateClientResponseDto();
        userClientResponse.isActive = false;

        when(userClient
                .getUserByExternalId(userExternalId))
                .thenReturn(userClientResponse);

        val expectedErrorMessage = MessageFormat.format("The user {0} must be active to create an activity", userClientResponse.uuid);
        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(getActivityCreationRequestDto())
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is(expectedErrorMessage));
        then(groupService).should(never()).createGroup(any());
    }

    @Test
    @DisplayName("[/ - POST] It should throw a bad request when workflow is not found")
    void testActivityCreationWithInvalidUserRole() {
        val userClientResponse = UserFixture.generateClientResponseDto();
        userClientResponse.role.remove(UserRoles.CREATOR);

        when(userClient
                .getUserByExternalId(userExternalId))
                .thenReturn(userClientResponse);

        val expectedErrorMessage = MessageFormat.format("The user {0} must have role {1} to create an activity", userClientResponse.uuid, UserRoles.CREATOR);
        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(getActivityCreationRequestDto())
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is(expectedErrorMessage));
        then(groupService).should(never()).createGroup(any());
    }

    @Test
    @DisplayName("[/ - POST] It should throw a bad request when workflow is not found")
    void testActivityCreationWithInvalidWorkflow() {
        val requestBody = getActivityCreationRequestDto();
        val workflowName = Faker.instance().funnyName().name();
        requestBody.setWorkflowName(workflowName);

        val expectedErrorMessage = MessageFormat.format("Workflow with name {0} not found", workflowName);
        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(requestBody)
                .when()
                .post()
                .then()
                .statusCode(Response.Status.BAD_REQUEST.getStatusCode())
                .body("errors[0]", is(expectedErrorMessage));
        then(groupService).should(never()).createGroup(any());
    }

    private CreateActivityRequestDtoV1 getActivityCreationRequestDto() {
        val requestDto = new CreateActivityRequestDtoV1();
        requestDto.setUserExternalId(userExternalId);
        requestDto.setWorkflowName(WorkflowStarter.GENERIC_WORKFLOW_NAME);

        return requestDto;
    }

// Add user into activity
    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] It should add user into activity")
    public void testAddUserIntoActivity() {
        val activityUuid = activityService.createActivity(userExternalId, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(getAddUserToActivityBody())
                .pathParam("activityUuid", activityUuid)
                .when()
                .post("/{activityUuid}/addUser")
                .then()
                .statusCode(Response.Status.OK.getStatusCode())
                .body(containsString(activityUuid.toString()), containsString(userExternalId))
                .body("participants", hasItem(userExternalId));

        then(activityService).should().addUserInActivity(activityUuid, userExternalId);
//        then(groupService).should().addUserToGroup(activityUuid, );
    }

    private AddUserToActivityRequestDtoV1 getAddUserToActivityBody() {
        return new AddUserToActivityRequestDtoV1(userExternalId);
    }
}
