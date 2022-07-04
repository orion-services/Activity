package dev.orion.broker.consumer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.DeliverCallback;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.DocumentEditDto;
import dev.orion.services.dto.ActivityExecutionDto;
import dev.orion.services.interfaces.ActivityService;
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

    static final String QUEUE_NAME = ConfigProvider.getConfig().getOptionalValue("rabbit.queue.consumer.document", String.class).get();
    static final Boolean AUTO_ACK = true;
    @Inject
    ActivityService activityService;
    @LoggerName("DocumentEditorConsumer")
    Logger logger;
    DeliverCallback deliverCallback = (consumerTag, delivery) -> {
        ObjectMapper objectMapper = new ObjectMapper();
        val documentEdit = objectMapper.readValue(delivery.getBody(), DocumentEditDto.class);
        logger.info(MessageFormat.format("Message received with {0}", documentEdit.toString()));
        try {
            val activityExecutionDto = new ActivityExecutionDto(documentEdit.activityId, documentEdit.documentId, documentEdit.externalUserId, documentEdit.documentContent);
            activityService.execute(activityExecutionDto);
        } catch (RuntimeException e) {
            logger.warnv("Activity {0} not let the user {1} edit document", documentEdit.activityId, documentEdit.externalUserId);
            e.printStackTrace();
        }

    };
    private Boolean hasStarted = false;

    public DocumentEditorConsumer() throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        this(QUEUE_NAME);
    }

    public DocumentEditorConsumer(String queue) throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        super(queue);
    }

    @PostConstruct
    void setupDependencies() {

    }

    public void attachQueueListener() throws IOException {
        if (!hasStarted) {
            logger.info(MessageFormat.format("Started listening queue: {0}", QUEUE_NAME));
            channel.basicConsume(QUEUE_NAME, AUTO_ACK, deliverCallback, consumerTag -> {
            });
            hasStarted = true;
        }
    }
}
