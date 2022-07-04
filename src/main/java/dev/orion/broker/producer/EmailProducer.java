package dev.orion.broker.producer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.AMQP;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.SendEmailProduceBody;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.eclipse.microprofile.config.ConfigProvider;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.text.MessageFormat;
import java.util.concurrent.TimeoutException;

@ApplicationScoped
public class EmailProducer extends RabbitConnection {
    @LoggerName("EmailProducer")
    Logger logger;

    static final String QUEUE_NAME = ConfigProvider.getConfig().getValue("rabbit.queue.producer.email", String.class);

    public EmailProducer() throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        super(QUEUE_NAME);
    }

    public void sendMessage(SendEmailProduceBody editorUpdateQueueDto) throws IOException {
        val messageProperties = new AMQP.BasicProperties.Builder().contentType("application/json").build();
        val objectMapper = new ObjectMapper();
        byte[] msg = objectMapper.writeValueAsBytes(editorUpdateQueueDto);

        channel.basicPublish("", QUEUE_NAME, messageProperties, msg);
        logger.info(MessageFormat.format("Emails ({0}) sent to queue {1}", editorUpdateQueueDto.getUserMessageMap(), QUEUE_NAME));
    }
}