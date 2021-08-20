package dev.orion.broker.consumer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.DeliverCallback;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.DocumentEditDto;
import dev.orion.broker.dto.DocumentUpdateDto;
import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.services.interfaces.DocumentService;
import io.quarkus.arc.log.LoggerName;
import org.eclipse.microprofile.config.ConfigProvider;
import org.jboss.logging.Logger;

import javax.annotation.PostConstruct;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.text.MessageFormat;
import java.util.concurrent.TimeoutException;

@ApplicationScoped
public class DocumentEditorConsumer extends RabbitConnection {

    @Inject
    DocumentService documentService;

    @Inject
    DocumentUpdateProducer documentUpdateProducer;

    @LoggerName("DocumentEditorConsumer")
    Logger logger;

    static final String QUEUE_NAME = ConfigProvider.getConfig().getValue("rabbit.queue.consumer.document", String.class);
    static final Boolean AUTO_ACK = true;
    private Boolean hasStarted = false;

    public DocumentEditorConsumer() throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        this(QUEUE_NAME);
    }

    public DocumentEditorConsumer(String queue) throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        super(queue);
    }

    DeliverCallback deliverCallback = (consumerTag, delivery) -> {
        ObjectMapper objectMapper = new ObjectMapper();
        var message = objectMapper.readValue(delivery.getBody(), DocumentEditDto.class);
        logger.info(MessageFormat.format("Message received with {0}", message.toString()));

        var newDocumentOption = documentService.editContent(message.documentContent, message.uuid, message.externalUserId);
        if (newDocumentOption.isPresent()) {
            var newDocument = newDocumentOption.get();
            var updatedDocumentDto = new DocumentUpdateDto();

            updatedDocumentDto.documentContent = newDocument.content;
            updatedDocumentDto.activityUuid = newDocument.activity.uuid;
            updatedDocumentDto.externalUserId = newDocument.editedBy.externalId;

            documentUpdateProducer.sendMessage(updatedDocumentDto);

            logger.info(MessageFormat.format("Document id({0}) sent to broker", newDocument.id));
        }
    };

    @PostConstruct
    void setupDependencies() {

    }

    public void attachQueueListener() throws IOException {
        if (!hasStarted) {
            logger.info(MessageFormat.format("Started listening queue: {0}", QUEUE_NAME));
            channel.basicConsume(QUEUE_NAME, AUTO_ACK, deliverCallback, consumerTag -> {});
            hasStarted = true;
        }
    }
}
