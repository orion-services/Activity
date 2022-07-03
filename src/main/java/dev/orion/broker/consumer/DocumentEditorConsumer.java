package dev.orion.broker.consumer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.DeliverCallback;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.DocumentEditDto;
import dev.orion.broker.producer.DocumentUpdateProducer;
import dev.orion.entity.Activity;
import dev.orion.services.dto.ActivityExecutionDto;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.services.interfaces.DocumentService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
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
    ActivityService activityService;

    @Inject
    DocumentUpdateProducer documentUpdateProducer;

    @LoggerName("DocumentEditorConsumer")
    Logger logger;

    static final String QUEUE_NAME = ConfigProvider.getConfig().getOptionalValue("rabbit.queue.consumer.document", String.class).get();
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
        val documentEdit = objectMapper.readValue(delivery.getBody(), DocumentEditDto.class);
        logger.info(MessageFormat.format("Message received with {0}", documentEdit.toString()));

        val activityExecutionDto = new ActivityExecutionDto(documentEdit.uuid, documentEdit.documentUUID, documentEdit.externalUserId, documentEdit.documentContent);

        activityService.execute(activityExecutionDto);

        logger.info(MessageFormat.format("Document id({0}) sent to broker", documentEdit.documentUUID));
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
