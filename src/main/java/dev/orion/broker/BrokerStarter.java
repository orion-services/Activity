package dev.orion.broker;

import dev.orion.broker.consumer.DocumentEditorConsumer;
import io.quarkus.runtime.StartupEvent;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;
import javax.inject.Inject;

import org.jboss.logging.Logger;

import java.io.IOException;


@ApplicationScoped
public class BrokerStarter {
    private static final Logger LOGGER = Logger.getLogger("ListenerBean");

    @Inject
    DocumentEditorConsumer documentEditorConsumer;

    void onStart(@Observes StartupEvent ev) {
        LOGGER.info("The application is starting...");
        try {
            documentEditorConsumer.attachQueueListener();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
