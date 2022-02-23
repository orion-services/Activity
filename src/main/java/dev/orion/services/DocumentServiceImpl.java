package dev.orion.services;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.DocumentService;
import dev.orion.util.exceptions.UserInvalidOperationException;
import io.quarkus.arc.log.LoggerName;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.Optional;
import java.util.UUID;

@ApplicationScoped
@Transactional
public class DocumentServiceImpl implements DocumentService {
    @Inject
    ActivityService activityService;

    @LoggerName("DocumentService")
    Logger logger;

    @Override
    public Optional<Document> editContent(String content, UUID activityUuid, String externalUserId) {
        if (activityService.canUserEditDocument(activityUuid, externalUserId)) {
            Optional<Document> documentOpt = Document.getDocumentByActivity(activityUuid);
            if (documentOpt.isPresent()) {
                var document = documentOpt.get();
                var user = User.findUserByExternalId(externalUserId).get();
                document.content = content;
                document.editedBy = user;
                try {
                    activityService.nextRound(activityUuid);
                } catch (UserInvalidOperationException userInvalidOperationException) {
                    logger.warn(userInvalidOperationException);
                }
                return Optional.of(document);
            }
        }

        return Optional.empty();
    }
}
