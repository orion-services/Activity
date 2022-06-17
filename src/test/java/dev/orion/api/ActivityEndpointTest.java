package dev.orion.api;


import dev.orion.api.endpoint.ActivityEndpoint;
import dev.orion.api.endpoint.body.AddUserToActivityRequestBody;
import dev.orion.api.endpoint.body.AddUserToActivityResponseBody;
import dev.orion.api.endpoint.body.CreateActivityRequestBody;
import dev.orion.api.endpoint.body.DefaultErrorResponseBody;
import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.ActivityStages;
import dev.orion.commom.constant.UserRoles;
import dev.orion.commom.constant.UserStatus;
import dev.orion.entity.Activity;
import dev.orion.fixture.UserFixture;
import dev.orion.services.UserServiceImpl;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import dev.orion.util.setup.WorkflowStarter;
import io.quarkus.panache.mock.PanacheMock;
import io.quarkus.test.common.http.TestHTTPEndpoint;
import io.quarkus.test.junit.QuarkusMock;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import io.quarkus.test.junit.mockito.InjectSpy;
import lombok.val;
import net.bytebuddy.utility.RandomString;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.*;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.Optional;
import java.util.UUID;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

@QuarkusTest
@TestHTTPEndpoint(ActivityEndpoint.class)
public class ActivityEndpointTest {
    final private String userUuid = RandomString.make();
    final private String userName = Faker.instance().name().fullName();
    @InjectMock
    @RestClient
    UserClient userClient;
    @InjectSpy
    ActivityService activityService;
    @InjectSpy
    GroupService groupService;

    @BeforeEach
    public void setup() {
        UserClientResponse userClientResponse = UserFixture.generateClientResponseDto();
        userClientResponse.uuid = userUuid;
        userClientResponse.name = userName;

        mockUserRequest(userClientResponse);
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
                .body("uuid", notNullValue(), "groups", is(Matchers.empty()))
                .extract()
                .path("uuid");

        then(activityService).should().createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val activity = (Activity) Activity.findById(UUID.fromString(uuid));
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
                .getUserByExternalId(userUuid))
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
                .getUserByExternalId(userUuid))
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

    private CreateActivityRequestBody getActivityCreationRequestDto() {
        val requestDto = new CreateActivityRequestBody();
        requestDto.setUserExternalId(userUuid);
        requestDto.setWorkflowName(WorkflowStarter.GENERIC_WORKFLOW_NAME);

        return requestDto;
    }

    // Add user into activity
    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] It should add user into activity")
    public void testAddUserIntoActivity() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val response = requestAddUserInActivity(activityUuid, AddUserToActivityResponseBody.class, Response.Status.OK.getStatusCode());

        Assertions.assertEquals(activityUuid, response.getUuid());
        Assertions.assertTrue(response.getParticipants().contains(userUuid));
        then(activityService).should().addUserInActivity(activityUuid, userUuid);
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if activity exists")
    public void testAddUserValidateActivityExists() {
        val randomUUID = UUID.randomUUID();
        val response = requestAddUserInActivity(randomUUID, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("Activity with UUID {0} not found", randomUUID);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if user is in another activity before try add")
    public void testAddUserAlreadyInAnotherActivity() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);

        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.getUserEntity().activity = new Activity();
        val userService = mockEnhancedUser(userEnhancedWithExternalData);

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity", userEnhancedWithExternalData.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
        BDDMockito.then(userService).should().getCompleteUserData(userUuid);
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if user is active before try add")
    public void testAddUserNotActive() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.isActive = false;
        val userService = mockEnhancedUser(userEnhancedWithExternalData);

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not ACTIVE", userEnhancedWithExternalData.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
        BDDMockito.then(userService).should().getCompleteUserData(userUuid);
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if user is AVAILABLE")
    public void testAddUserNotAvailable() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.status = UserStatus.CONNECTED;
        mockEnhancedUser(userEnhancedWithExternalData);

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not AVAILABLE", userEnhancedWithExternalData.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must format the exception message when validation catch something on add user ")
    public void testAddUserExceptionFormatWithMultipleErrors() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        userEnhancedWithExternalData.getUserEntity().activity = new Activity();
        userEnhancedWithExternalData.status = UserStatus.CONNECTED;
        userEnhancedWithExternalData.isActive = false;

        mockEnhancedUser(userEnhancedWithExternalData);

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity, it is not AVAILABLE and it is not ACTIVE", userEnhancedWithExternalData.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if it is active before add user")
    public void testAddUserValidateActivityIsActive() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();

        mockEnhancedUser(userEnhancedWithExternalData);

        Activity activity = Activity.findById(activityUuid);
        activity.isActive = false;

        PanacheMock.mock(Activity.class);
        when(Activity.findByIdOptional(any())).thenReturn(Optional.of(activity));

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, userEnhancedWithExternalData.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if it has not started before add user")
    public void testAddUserValidateActivityHasNotStarted() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        val userEnhancedWithExternalData = UserFixture.generateUserEnhancedWithExternalDataDto();
        mockEnhancedUser(userEnhancedWithExternalData);
        Activity activity = Activity.findById(activityUuid);
        activity.setActualStage(ActivityStages.DURING);

        PanacheMock.mock(Activity.class);
        when(Activity.findByIdOptional(any())).thenReturn(Optional.of(activity));

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", userEnhancedWithExternalData.uuid, activityUuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    private <T> T requestAddUserInActivity(UUID activityUuid, Class<T> responseClass, int expectedStatusCode) {
        return given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(new AddUserToActivityRequestBody(userUuid))
                .pathParam("activityUuid", activityUuid)
                .when()
                .post("/{activityUuid}/addUser")
                .then()
                .statusCode(expectedStatusCode)
                .extract()
                .body()
                .as(responseClass);
    }

    private void mockUserRequest(UserClientResponse user) {
        when(userClient.getUserByExternalId(userUuid))
                .thenReturn(user);
    }

    private UserService mockEnhancedUser(UserEnhancedWithExternalData user) {
        val userService = Mockito.mock(UserServiceImpl.class);
        QuarkusMock.installMockForType(userService, UserServiceImpl.class);

        BDDMockito
                .given(userService.getCompleteUserData(anyString()))
                .willReturn(user);

        return userService;
    }
}
