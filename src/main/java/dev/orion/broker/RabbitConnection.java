package dev.orion.broker;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import com.rabbitmq.client.impl.nio.NioParams;
import org.eclipse.microprofile.config.ConfigProvider;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;
import java.util.concurrent.TimeoutException;


public abstract class RabbitConnection {
    protected static ConnectionFactory factory = new ConnectionFactory();
    protected static Connection connection;
    protected Channel channel;
    protected String queueName;




    protected RabbitConnection(String queueName) throws IOException, TimeoutException {
        var profile = ConfigProvider.getConfig().getValue("quarkus.profile", String.class);
        this.queueName = queueName;
        if (!profile.equals("test")) {
            setup();
        }
    }

    private void setup() throws IOException, TimeoutException {
        if (connection == null) {
            setLocalHost();
            connection = factory.newConnection();
            factory.useNio();
            factory.setNioParams(new NioParams().setNbIoThreads(4));
        }

        this.channel = connection.createChannel();

        channel.queueDeclare(queueName,false,false,false,null);
    }

    public void close() throws IOException, TimeoutException {
        this.channel.close();
        connection.close();
    }

    private void setLocalHost() {
        factory.setUsername(ConfigProvider.getConfig().getValue("rabbit.username", String.class));
        factory.setPassword(ConfigProvider.getConfig().getValue("rabbit.password",String.class));
        factory.setVirtualHost(ConfigProvider.getConfig().getValue("rabbit.virtualHost",String.class));
        factory.setHost(ConfigProvider.getConfig().getValue("rabbit.host",String.class));
        factory.setPort(ConfigProvider.getConfig().getValue("rabbit.port",Integer.class));
    }
}