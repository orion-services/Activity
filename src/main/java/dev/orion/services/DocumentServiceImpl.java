package dev.orion.services;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.DocumentService;
import io.quarkus.arc.log.LoggerName;
import org.jboss.logging.Logger;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;
import java.util.Optional;
import java.util.UUID;

@RequestScoped
@Transactional
public class DocumentServiceImpl implements DocumentService {
    @Inject
    ActivityService activityService;

    @LoggerName("DocumentService")
    Logger fooLog;

    @Override
    public Boolean editContent(String content, UUID activityUuid, String externalUserId) {
        if (Boolean.TRUE.equals(activityService.canUserEditDocument(activityUuid, externalUserId))) {
            Optional<Document> documentOpt = Document.getDocumentByActivity(activityUuid);
            if (documentOpt.isPresent()) {
                var document = documentOpt.get();
                document.content = content;
                activityService.nextRound(activityUuid);
                return true;
            }
        }

        return false;
    }
}
