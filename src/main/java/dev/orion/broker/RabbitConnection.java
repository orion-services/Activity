package dev.orion.broker;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import org.eclipse.microprofile.config.ConfigProvider;
import org.jboss.logging.Logger;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.Optional;
import java.util.concurrent.TimeoutException;


public abstract class RabbitConnection {
    protected static ConnectionFactory factory = new ConnectionFactory();
    protected static Connection connection;
    protected Channel channel;
    protected String queueName;
    private static final Logger LOG = Logger.getLogger(RabbitConnection.class);


    protected RabbitConnection(String queueName) throws IOException, TimeoutException, URISyntaxException, NoSuchAlgorithmException, KeyManagementException {
        this.queueName = queueName;
        final Optional<String> optHostString = ConfigProvider.getConfig().getOptionalValue("rabbit.host",String.class);
        if (optHostString.isEmpty()) {
            LOG.warn("Hosting of rabbitMq empty, not connecting to the broker");
            return;
        }

        setupConnectionAndChannel(optHostString.get());
    }

    public void close() throws IOException, TimeoutException {
        this.channel.close();
        connection.close();
    }

    private void setupConnectionAndChannel(String host) throws URISyntaxException, NoSuchAlgorithmException, KeyManagementException, IOException, TimeoutException {
        if (connection == null) {
            factory.setUri(host);
            connection = factory.newConnection();
        }

        this.channel = connection.createChannel();

        channel.queueDeclare(queueName,false,false,false,null);
    }

    private void setLocalHost() {
        factory.setUsername(ConfigProvider.getConfig().getValue("rabbit.username", String.class));
        factory.setPassword(ConfigProvider.getConfig().getValue("rabbit.password",String.class));
        factory.setVirtualHost(ConfigProvider.getConfig().getValue("rabbit.virtualHost",String.class));
        factory.setHost(ConfigProvider.getConfig().getValue("rabbit.host",String.class));
        factory.setPort(ConfigProvider.getConfig().getValue("rabbit.port",Integer.class));
    }
}
