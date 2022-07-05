package dev.orion.workflowExecutor.impl;

import dev.orion.broker.producer.EmailProducer;
import dev.orion.broker.dto.SendEmailProduceBody;
import dev.orion.commom.exception.InvalidWorkflowConfiguration;
import dev.orion.commom.exception.NotValidActionException;
import dev.orion.entity.*;
import dev.orion.entity.step_type.SendEmailStep;
import dev.orion.workflowExecutor.StepExecutor;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;

@ApplicationScoped
@Transactional
public class SendEmailStepExecutor implements StepExecutor {
    @LoggerName("SendEmailStepExecutor")
    Logger logger;

    @Inject
    EmailProducer emailProducer;

    @Override
    public void execute(Document document, User user, Step step) {
        logger.infov("Executing step {0} ", getStepRepresentation());
        val sendEmailStep = (SendEmailStep) step;
        val activity = Optional
                .ofNullable(user.getActivity())
                .orElseGet(() -> Activity.findByCreator(user.getExternalId()).orElseThrow());

        val actualStage = activity.getActualStage();

        val emailMessage = sendEmailStep.getActivityStageMessageMap().get(actualStage);
        val sendEmailRequestBody = createEmailProduceBody(activity, sendEmailStep, emailMessage);

        try {
            emailProducer.sendMessage(sendEmailRequestBody);
        } catch (IOException e) {

            e.printStackTrace();
        }
    }

    SendEmailProduceBody createEmailProduceBody(Activity activity, SendEmailStep sendEmailStep, String emailMessage) {
        val sendEmailRequest = new SendEmailProduceBody();
        val participants = sendEmailStep.isOnlyForCreator() ? new HashSet<User>() : activity.getParticipants();
        participants.add(activity.getCreator());

        participants.forEach(user -> {
            sendEmailRequest.addMessage(user.externalId, emailMessage);
        });

        return sendEmailRequest;
    }

    @Override
    public void validate(Document document, User user, Step step) {
        val sendEmailStep = (SendEmailStep) step;
        val activity = Optional
                .ofNullable(user.getActivity())
                .orElseGet(() -> Activity.findByCreator(user.getExternalId()).orElseThrow());
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
                .filter(stepFilter -> getStepRepresentation().equals(stepFilter.getStepType()))
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
