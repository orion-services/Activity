package dev.orion.broker.producer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.AMQP;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.ActivityUpdateMessageDto;
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
public class ActivityUpdateProducer extends RabbitConnection {
    @LoggerName("ActivityUpdateProducer")
    Logger logger;

    static final String QUEUE_NAME = ConfigProvider.getConfig().getValue("rabbit.queue.producer.activity", String.class);

    public ActivityUpdateProducer() throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        super(QUEUE_NAME);
    }

    public void sendMessage(ActivityUpdateMessageDto editorUpdateQueueDto) throws IOException {
        val messageProperties = new AMQP.BasicProperties.Builder().contentType("application/json").build();
        val objectMapper = new ObjectMapper();
        byte[] msg = objectMapper.writeValueAsBytes(editorUpdateQueueDto);

        channel.basicPublish("", QUEUE_NAME, messageProperties, msg);
        logger.info(MessageFormat.format("Activity ({0}) update sent to queue {1}", editorUpdateQueueDto.uuid, QUEUE_NAME));
    }
}