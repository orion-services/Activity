package dev.orion.broker.producer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.AMQP;
import dev.orion.broker.RabbitConnection;
import dev.orion.broker.dto.DocumentUpdateDto;
import dev.orion.entity.Document;
import dev.orion.entity.User;
import org.eclipse.microprofile.config.ConfigProvider;

import javax.enterprise.context.ApplicationScoped;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.TimeoutException;

@ApplicationScoped
public class DocumentUpdateProducer extends RabbitConnection {

    static final String QUEUE_NAME = ConfigProvider.getConfig().getValue("rabbit.queue.producer.document", String.class);

    public DocumentUpdateProducer() throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        this(QUEUE_NAME);
    }


    public DocumentUpdateProducer(String QUEUE_NAME) throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        super(QUEUE_NAME);
    }

    public void sendMessage(DocumentUpdateDto documentUpdateDto) throws IOException {
        AMQP.BasicProperties messageProperties = new AMQP.BasicProperties.Builder().contentType("application/json").build();
        ObjectMapper objectMapper = new ObjectMapper();
        byte[] msg = objectMapper.writeValueAsBytes(documentUpdateDto);

        channel.basicPublish("", QUEUE_NAME, messageProperties, msg);
    }
}
