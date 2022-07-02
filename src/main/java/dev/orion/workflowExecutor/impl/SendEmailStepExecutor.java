package dev.orion.workflowExecutor.impl;

import dev.orion.client.EmailClient;
import dev.orion.client.dto.SendEmailRequest;
import dev.orion.commom.exception.InvalidWorkflowConfiguration;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.SendEmailStep;
import dev.orion.workflowExecutor.StepExecutor;
import lombok.val;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Objects;

@ApplicationScoped
public class SendEmailStepExecutor implements StepExecutor {

    @Inject
    @RestClient
    EmailClient emailClient;

    @Override
    public void execute(Document document, User user, Step step) {
        val sendEmailStep = (SendEmailStep) step;
        val activity = user.getActivity();
        val actualStage = activity.getActualStage();

        val emailMessage = sendEmailStep.getActivityStageMessageMap().get(actualStage);
        val sendEmailRequestBody = createSendRequest(activity, sendEmailStep, emailMessage);

        emailClient.sendEmails(sendEmailRequestBody);
    }

    SendEmailRequest createSendRequest(Activity activity, SendEmailStep sendEmailStep, String emailMessage) {
        val sendEmailRequest = new SendEmailRequest();
        val participants = sendEmailStep.isOnlyForCreator() ? new HashSet<User>() : activity.getUserList();
        participants.add(activity.getCreator());

        participants.forEach(user -> {
            sendEmailRequest.addMessage(user.externalId, emailMessage);
        });

        return sendEmailRequest;
    }

    @Override
    public void validate(Document document, User user, Step step) {
        val sendEmailStep = (SendEmailStep) step;
        val activity = user.getActivity();
        val messageFromStage = sendEmailStep.getActivityStageMessageMap().get(activity.getActualStage());

        if (Objects.isNull(messageFromStage)) {
            val message = MessageFormat.format("The email message of stage {0} can''t be null", activity.getActualStage());
            throw new InvalidWorkflowConfiguration(message);
        }
    }

    @Override
    public boolean isFinished(Activity activity, Step step) throws NotValidActionException {
        return true;
    }

    @Override
    public void validateConfig(Stage stage) {
        val step = stage.getSteps().stream()
                .filter(stepFilter -> stepFilter.getStepType() == getStepRepresentation())
                .findFirst()
                .orElseThrow(() -> {
                    throw new InvalidWorkflowConfiguration(MessageFormat.format("There is no step {0} on the stage with ID {1}", getStepRepresentation(), stage.id));
                });
        if (!step.getAllowedStages().contains(stage.getActivityStage())) {
            val message = MessageFormat.format("The step {0} can be placed only in stages {1}", step.getStepType(), step.getAllowedStages());
            throw new InvalidWorkflowConfiguration(message);
        }
    }

    @Override
    public String getStepRepresentation() {
        return new SendEmailStep().getStepType();
    }
}
