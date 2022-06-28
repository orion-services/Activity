package dev.orion.workflowExecutor;

import dev.orion.entity.*;
import dev.orion.entity.step_type.UnorderedCircleOfWriters;
import dev.orion.services.interfaces.DocumentService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;

@ApplicationScoped
public class UnorderedCircleOfWriterStepExecutor implements StepExecutor {
    @LoggerName("UnorderedCircleOfWriterStepExecutor")
    private Logger logger;
    @Inject
    private DocumentService documentService;

    @Override
    public void execute(Activity activity, User user, Step step) {
        val unorderedCircleOfWriter = (UnorderedCircleOfWriters) step;

        val groupActivity = user.getGroupActivity();
        val documentList = Document.findAllByUserIdAndGroup(user.externalId, groupActivity.getUuid());
        if (documentList.isEmpty()) {
            logger.warnv("User {0} is not found in group {1}", user.getExternalId(), groupActivity.getUuid());
            return;
        }
        val document = documentList.get(0);

        documentService.moveParticipantToEditedList(document, user);
        advanceDocumentRoundAndResetForNextRound(document, groupActivity,   unorderedCircleOfWriter);
    }

    private void advanceDocumentRoundAndResetForNextRound(Document document, GroupActivity groupActivity, UnorderedCircleOfWriters unorderedCircleOfWriters) {
        if (isFinalRound(document, unorderedCircleOfWriters)) {
            return;
        }

        if (doesAllParticipantsHaveParticipated(document, groupActivity)) {
            val newRoundValue = document.getRounds() + 1;
            logger.infov("The document {0} will advance the round from {1} to {2}", document.getExternalId(), document.getRounds(), newRoundValue);
            document.setRounds(newRoundValue);
            documentService.moveAllUsersFromEditedToParticipantList(document.getExternalId());
        }

    }

    private boolean isFinalRound(Document document, UnorderedCircleOfWriters unorderedCircleOfWriters) {
        return unorderedCircleOfWriters.getRounds() <= document.getRounds();
    }

    private boolean doesAllParticipantsHaveParticipated(Document document, GroupActivity groupActivity) {
        return document.getParticipantsAssigned().isEmpty() && document.getParticipantsThatEdited().containsAll(groupActivity.getParticipants());
    }

    @Override
    public void validate(Activity activity, User user, Step step) {
        val unorderedCircleOfWriter = (UnorderedCircleOfWriters) step;
        throw new RuntimeException("Should implement it");
    }

    @Override
    public String getStepRepresentation() {
        return new UnorderedCircleOfWriters().getStepType();
    }
}
