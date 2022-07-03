package dev.orion.workflowExecutor;

import dev.orion.client.EmailClient;
import dev.orion.client.dto.SendEmailRequest;
import dev.orion.commom.constant.ActivityStage;
import dev.orion.commom.exception.InvalidWorkflowConfiguration;
import dev.orion.entity.*;
import dev.orion.entity.step_type.SendEmailStep;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.workflowExecutor.impl.SendEmailStepExecutor;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import lombok.val;
import net.datafaker.Faker;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.hibernate.Session;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.*;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

import static dev.orion.fixture.ActivityFixture.generateActivity;
import static dev.orion.fixture.GroupFixture.createGroup;
import static dev.orion.fixture.GroupFixture.generateDocument;
import static dev.orion.fixture.UserFixture.createParticipants;
import static dev.orion.fixture.UserFixture.generateUser;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.mock;

@QuarkusTest
public class SendEmailStepStepExecutorTest {
    @Inject
    SendEmailStepExecutor testThis;

    @InjectMock
    Session session;

    @RestClient
    @InjectMock
    EmailClient emailClient;

    private User userCreator;

    private Activity usingActivity;

    private GroupActivity usingGroup;

    private LinkedHashSet<User> usingParticipants;

    private Document usingDocument;

    private SendEmailStep usingStep;

    final private String preStageEmailMessage = Faker.instance().yoda().quote();

    final private String posStageEmailMessage = Faker.instance().bossaNova().song();

    @BeforeEach
    private void setup() {
        MockitoAnnotations.openMocks(this);

        usingStep = createSendEmailStep();

        userCreator = generateUser();

        usingActivity = generateActivity(userCreator);

        usingParticipants = createParticipants(usingActivity, 5);

        usingDocument = generateDocument(usingParticipants);

        usingGroup = createGroup(usingActivity, usingDocument, usingParticipants);
    }

    private SendEmailStep createSendEmailStep() {
        val sendEmail = new SendEmailStep();
        sendEmail.addMessage(ActivityStage.PRE, preStageEmailMessage);
        sendEmail.addMessage(ActivityStage.POS, posStageEmailMessage);

        return sendEmail;
    }

    @Test
    @DisplayName("[getStepRepresentation] - Should return the CircleOfWriters name")
    public void testGetStepRepresentationReturn() {
        val stepRepresentation = testThis.getStepRepresentation();

        Assertions.assertEquals(new SendEmailStep().getStepType(), stepRepresentation);
    }

    @Test
    @DisplayName("[finished] - The method doesn't really has a finishing")
    public void testFinishingIsReturningTrue() {
        val finished = testThis.isFinished(usingActivity, usingStep);
        Assertions.assertTrue(finished);
    }

    @Test
    @DisplayName("[execute] - Should send e-mail for each activity participant")
    public void testNormalExecution() {
        testThis.execute(usingDocument, userCreator, usingStep);

        val requestBody = createSendRequest(usingParticipants, preStageEmailMessage);

        val sendEmailRequestArgumentCaptor = ArgumentCaptor.forClass(SendEmailRequest.class);
        then(emailClient).should().sendEmails(sendEmailRequestArgumentCaptor.capture());

        Assertions.assertEquals(requestBody.getUserMessageMap(), sendEmailRequestArgumentCaptor.getValue().getUserMessageMap());
    }

    @Test
    @DisplayName("[execute] - Should send e-mail only for the activity creator")
    public void testExecutionToONly() {
        usingStep.setOnlyForCreator(true);
        testThis.execute(usingDocument, userCreator, usingStep);
        val requestBody = createSendRequest(Set.of(userCreator), preStageEmailMessage);

        val sendEmailRequestArgumentCaptor = ArgumentCaptor.forClass(SendEmailRequest.class);
        then(emailClient).should().sendEmails(sendEmailRequestArgumentCaptor.capture());

        val userMessageMap = sendEmailRequestArgumentCaptor.getValue().getUserMessageMap();
        Assertions.assertEquals(requestBody.getUserMessageMap(), userMessageMap);
        Assertions.assertEquals(1, userMessageMap.size());
    }

    @Test
    @DisplayName("[execute] - Should send e-mail for chosen stage")
    public void testChosenStageExecution() {
        usingActivity.setActualStage(ActivityStage.POS);
        testThis.execute(usingDocument, userCreator, usingStep);

        val requestBody = createSendRequest(usingParticipants, posStageEmailMessage);

        val sendEmailRequestArgumentCaptor = ArgumentCaptor.forClass(SendEmailRequest.class);
        then(emailClient).should().sendEmails(sendEmailRequestArgumentCaptor.capture());

        Assertions.assertEquals(requestBody.getUserMessageMap(), sendEmailRequestArgumentCaptor.getValue().getUserMessageMap());
    }

    private @NotNull SendEmailRequest createSendRequest(Set<User> users, String message) {
        val sendEmailRequest = new SendEmailRequest();
        val participants = new HashSet<>(users);
        participants.add(usingActivity.getCreator());

        participants.forEach(user -> {
            sendEmailRequest.addMessage(user.externalId, message);
        });

        return sendEmailRequest;
    }

    @Test
    @DisplayName("[validateConfig] - Should validate the configuration of step")
    public void testChosenStepStageConfigValidation() {
        val stage = new Stage();
        stage.addStep(usingStep);

        stage.setActivityStage(ActivityStage.PRE);
        testThis.validateConfig(stage);

        stage.setActivityStage(ActivityStage.POS);
        testThis.validateConfig(stage);
    }

    @Test
    @DisplayName("[validateConfig] - Should throw when step is saved with invalid stages")
    public void testChosenInvalidStepStageConfigValidation() {
        val stage = new Stage();
        stage.addStep(usingStep);
        stage.setActivityStage(ActivityStage.DURING);

        val exceptionMessage = Assertions.assertThrows(InvalidWorkflowConfiguration.class, () -> testThis.validateConfig(stage)).getMessage();
        val expectedMessage = MessageFormat.format("The step {0} can be placed only in stages {1}", usingStep.getStepType(), usingStep.getAllowedStages());

        Assertions.assertEquals(expectedMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[validateConfig] - Should throw when step is saved with invalid stages")
    public void testPassingWrongStep() {
        val stage = new Stage();
        stage.id = 1L;
        stage.addStep(mock(Step.class));

        val exceptionMessage = Assertions.assertThrows(InvalidWorkflowConfiguration.class, () -> testThis.validateConfig(stage)).getMessage();
        val expectedMessage = MessageFormat.format("There is no step {0} on the stage with ID {1}", usingStep.getStepType(), stage.id);

        Assertions.assertEquals(expectedMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[validate] - Validate if there is message in PRE")
    public void testValidationOfMessage() {
        usingStep.getActivityStageMessageMap().put(ActivityStage.PRE, null);
        val exceptionMessage = Assertions.assertThrows(InvalidWorkflowConfiguration.class,
                () -> testThis.validate(null, userCreator, usingStep)).getMessage();
        val expectedMessage = MessageFormat.format("The email message of stage {0} can''t be null", usingActivity.getActualStage());

        Assertions.assertEquals(expectedMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[validate] - Validate if there is message in POS")
    public void testValidationOfMessagePosStage() {
        usingStep.getActivityStageMessageMap().put(ActivityStage.POS, null);
        usingActivity.setActualStage(ActivityStage.POS);
        val exceptionMessage = Assertions.assertThrows(InvalidWorkflowConfiguration.class,
                () -> testThis.validate(null, userCreator, usingStep)).getMessage();
        val expectedMessage = MessageFormat.format("The email message of stage {0} can''t be null", usingActivity.getActualStage());

        Assertions.assertEquals(expectedMessage, exceptionMessage);
    }

    @Test
    @DisplayName("[validate] - Should not throw when message is not null")
    public void testValidOfMessageValidation() {
        testThis.validate(null, userCreator, usingStep);
    }
}
