package dev.orion.api;


import dev.orion.api.endpoint.ActivityEndpoint;
import dev.orion.api.endpoint.body.*;
import dev.orion.client.DocumentClient;
import dev.orion.client.UserClient;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.constant.UserStatus;
import dev.orion.entity.Activity;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import dev.orion.fixture.ActivityFixture;
import dev.orion.fixture.UserFixture;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.GroupService;
import dev.orion.services.interfaces.UserService;
import dev.orion.services.interfaces.WorkflowManageService;
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
import org.hibernate.Session;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import javax.transaction.Transactional;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.*;

@QuarkusTest
@TestHTTPEndpoint(ActivityEndpoint.class)
public class ActivityEndpointTest {
    final private String userUuid = RandomString.make();
    final private String userName = Faker.instance().name().fullName();
    @InjectMock
    @RestClient
    UserClient userClient;

    @InjectMock
    @RestClient
    DocumentClient documentClient;
    @InjectSpy
    ActivityService activityService;
    @InjectSpy
    GroupService groupService;

    @InjectMock
    UserService userService;

    @InjectSpy
    WorkflowManageService workflowManageService;

    private UserEnhancedWithExternalData userCreator;

    private UserClientResponse userClientResponse;
    private Session session;

    @BeforeEach
    public void setup() {
        userCreator = UserFixture.mockEnhancedUser(userService, userUuid);
        mockUserRequest();
        BDDMockito.given(documentClient.createDocument(any())).willReturn(new CreateDocumentResponse());
    }

    private void mockUserRequest() {
        userClientResponse = UserFixture.generateClientResponseDto();
        userClientResponse.uuid = userUuid;
        userClientResponse.name = userName;
        when(userClient.getUserByExternalId(userUuid))
                .thenReturn(userClientResponse);
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
        Assertions.assertNotNull(activity);
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
        userCreator.isActive = false;

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
        val activityCreationRequestDto = getActivityCreationRequestDto();
        activityCreationRequestDto.setWorkflowName("");

        val expectedErrorMessage = "Workflow may not be blank";
        given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(activityCreationRequestDto)
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
    @Transactional
    public void testAddUserIntoActivity() {
        mockHibernateSession();

        val activity = mockActivityCreation();
        val activityUuid = activity.uuid;
        userCreator.getUserEntity().setActivity(null);

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
        Mockito.clearInvocations(userService);

        userCreator.getUserEntity().activity = new Activity();

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity", userCreator.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
        BDDMockito.then(userService).should().getCompleteUserData(userUuid);
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if user is active before try add")
    public void testAddUserNotActive() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        Mockito.clearInvocations(userService);
        userCreator.isActive = false;

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is not ACTIVE", userCreator.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
        BDDMockito.then(userService).should().getCompleteUserData(userUuid);
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must format the exception message when validation catch something on add user ")
    public void testAddUserExceptionFormatWithMultipleErrors() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        userCreator.getUserEntity().activity = new Activity();
        userCreator.status = UserStatus.CONNECTED;
        userCreator.isActive = false;

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("User {0} is not valid to join activity because: it is already in another activity and it is not ACTIVE", userCreator.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if it is active before add user")
    public void testAddUserValidateActivityIsActive() {
        mockHibernateSession();

        val activity = mockActivityCreation();
        val activityUuid = activity.uuid;
        activity.isActive = false;

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("Activity {0} must be active to add user {1}", activityUuid, userCreator.uuid);
        Assertions.assertTrue(response.getErrors().contains(expectedExceptionMessage));
    }

    @Test
    @DisplayName("[/{activityUuid}/addUser - POST] Activity must validate if it has not started before add user")
    public void testAddUserValidateActivityHasNotStarted() {
        val activityUuid = activityService.createActivity(userUuid, WorkflowStarter.GENERIC_WORKFLOW_NAME);
        Activity activity = Activity.findById(activityUuid);
        activity.setActualStage(ActivityStage.DURING);

        PanacheMock.mock(Activity.class);
        when(Activity.findByIdOptional(any())).thenReturn(Optional.of(activity));

        val response = requestAddUserInActivity(activityUuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedExceptionMessage = MessageFormat.format("Cannot add user {0} to Activity {1} because it has already start", userCreator.uuid, activityUuid);
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

    @Test
    @DisplayName("[/{activityUuid}/start - PATCH] Start application")
    public void testApplicationStart() {
        mockHibernateSession();
        val groupUUID = UUID.randomUUID();
        val activity = mockActivityCreation();

        User userEntity = userCreator.getUserEntity();
        userEntity.setStatus(UserStatus.CONNECTED);
        activity.addParticipant(userEntity);

        BDDMockito.doAnswer(injectIdIntoGroup(groupUUID)).when(session).persist(any(activity.getClass()));

        val responseBody = requestStartActivity(activity.uuid, StartActivityResponseBody.class, Response.Status.OK.getStatusCode());
        GroupActivity groupActivity = activity.getGroupActivities().get(0);

        Assertions.assertEquals(activity.uuid, responseBody.getActivityUUID());
        Assertions.assertTrue(responseBody.getGroups().containsKey(groupActivity.getUuid()));
        Assertions.assertTrue(responseBody.getGroups().get(groupActivity.getUuid()).contains(userEntity.externalId));
        then(documentClient).should().createDocument(any());
        then(groupService).should().createGroup(activity, activity.getUserList());
        then(workflowManageService).should().apply(activity, activity.getCreator(), null);
    }

    @Test
    @DisplayName("[/{activityUuid}/start - PATCH] Should not start when has no users in activity")
    public void testApplicationStartValidationOfEmptyActivity() {
        mockHibernateSession();
        val groupUUID = UUID.randomUUID();
        val activity = mockActivityCreation();

        User userEntity = userCreator.getUserEntity();
        userEntity.setStatus(UserStatus.CONNECTED);

        BDDMockito.doAnswer(injectIdIntoGroup(groupUUID)).when(session).persist(any(activity.getClass()));

        val responseBody = requestStartActivity(activity.uuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedMessage = MessageFormat.format("Activity {0} has no participants to start", activity.getUuid());
        Assertions.assertTrue(responseBody.getErrors().contains(expectedMessage));
        then(documentClient).should(never()).createDocument(any());
        then(groupService).should(never()).createGroup(any(), any());
    }

    @Test
    @DisplayName("[/{activityUuid}/start - PATCH] Should not start inactive activity")
    public void testApplicationStartValidationOfInactiveActivity() {
        mockHibernateSession();
        val groupUUID = UUID.randomUUID();
        val activity = mockActivityCreation();
        activity.setIsActive(false);

        User userEntity = userCreator.getUserEntity();
        userEntity.setStatus(UserStatus.CONNECTED);

        BDDMockito.doAnswer(injectIdIntoGroup(groupUUID)).when(session).persist(any(activity.getClass()));

        val responseBody = requestStartActivity(activity.uuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedMessage = MessageFormat.format("Activity {0} must be active", activity.uuid);
        Assertions.assertTrue(responseBody.getErrors().contains(expectedMessage));
        then(documentClient).should(never()).createDocument(any());
        then(groupService).should(never()).createGroup(any(), any());
    }

    @Test
    @DisplayName("[/{activityUuid}/start - PATCH] Should not start when has no connected user")
    public void testApplicationStartValidationIfThereAreNotConnectedUser() {
        mockHibernateSession();
        val groupUUID = UUID.randomUUID();
        val activity = mockActivityCreation();

        val userEntity = userCreator.getUserEntity();
        userEntity.setStatus(UserStatus.CONNECTED);
        val notConnectedUser = UserFixture.generateUser();

        activity.addParticipant(userEntity);
        activity.addParticipant(notConnectedUser);

        BDDMockito.doAnswer(injectIdIntoGroup(groupUUID)).when(session).persist(any(activity.getClass()));

        val responseBody = requestStartActivity(activity.uuid, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedMessage = MessageFormat.format("Activity {0} has the following users not connected: {1}", activity.uuid, List.of(notConnectedUser.getExternalId()));
        Assertions.assertTrue(responseBody.getErrors().contains(expectedMessage));
        then(documentClient).should(never()).createDocument(any());
        then(groupService).should(never()).createGroup(any(), any());
    }

    private Answer injectIdIntoGroup(UUID groupUUID) {
        return invocation -> {
            val activityParam = (Activity) invocation.getArgument(0);
            activityParam.getGroupActivities().get(0).setUuid(groupUUID);
            return null;
        };
    }

    @Test
    @DisplayName("[/{activityUuid}/start - PATCH] Should validate if activity exists")
    public void testApplicationStartNotFoundActivity() {
        val activityUUID = UUID.randomUUID();

        val responseBody = requestStartActivity(activityUUID, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());

        val expectedMessage = MessageFormat.format("Activity {0} not found", activityUUID);

        Assertions.assertTrue(responseBody.getErrors().contains(expectedMessage));
        then(documentClient).should(never()).createDocument(any());
        then(groupService).should(never()).createGroup(any(), any());
    }

    private <T> T requestStartActivity(UUID activityUuid, Class<T> responseClass, int expectedStatusCode) {
        return given()
                .contentType(MediaType.APPLICATION_JSON)
                .body(new AddUserToActivityRequestBody(userUuid))
                .pathParam("activityUuid", activityUuid)
                .when()
                .patch("/{activityUuid}/start")
                .then()
                .statusCode(expectedStatusCode)
                .extract()
                .body()
                .as(responseClass);
    }

    @Test
    @DisplayName("[/{activityUuid} - GET] Should get activity By UUID")
    public void testFindActivity() {
        mockHibernateSession();
        val activity = mockActivityCreation();
        val responseBody = requestFindActivity(activity.uuid, Activity.class, Response.Status.OK.getStatusCode());

        Assertions.assertEquals(activity.uuid, responseBody.uuid);
    }

    @Test
    @DisplayName("[/{activityUuid} - GET] Should throw when not find the activity")
    public void testFindActivityWithoutActivity() {
        mockHibernateSession();
        val invalidActivityUUID = UUID.randomUUID();

        val responseBody = requestFindActivity(invalidActivityUUID, DefaultErrorResponseBody.class, Response.Status.BAD_REQUEST.getStatusCode());
        val expectedMessage = MessageFormat.format("Activity {0} not found", invalidActivityUUID);
        Assertions.assertTrue(responseBody.getErrors().contains(expectedMessage));
    }

    private <T> T requestFindActivity(UUID activityUuid, Class<T> responseClass, int expectedStatusCode) {
        return given()
                .contentType(MediaType.APPLICATION_JSON)
                .pathParam("activityUuid", activityUuid)
                .when()
                .get("/{activityUuid}")
                .then()
                .statusCode(expectedStatusCode)
                .extract()
                .body()
                .as(responseClass);
    }


    private void mockHibernateSession() {
        session = Mockito.mock(Session.class);
        QuarkusMock.installMockForType(session, Session.class);
        BDDMockito.doNothing().when(session).persist(any());
    }

    private Activity mockActivityCreation() {
        Activity activity = ActivityFixture.generateActivity(userCreator.getUserEntity());

        PanacheMock.mock(Activity.class);
        when(Activity.findByIdOptional(any())).thenReturn(Optional.of(activity));
        when(Activity.findById(any())).thenReturn(activity);

        return activity;
    }
}
