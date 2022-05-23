package dev.orion.broker;

import dev.orion.broker.consumer.DocumentEditorConsumer;
import io.quarkus.runtime.StartupEvent;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;
import javax.inject.Inject;

import io.quarkus.runtime.configuration.ProfileManager;
import org.jboss.logging.Logger;

import java.io.IOException;


@ApplicationScoped
public class BrokerStarter {
    Logger LOGGER = Logger.getLogger(BrokerStarter.class);

    @Inject
    DocumentEditorConsumer documentEditorConsumer;

    void onStart(@Observes StartupEvent ev) {
        LOGGER.info(String.format("The application is starting with profile: %s", ProfileManager.getActiveProfile()));
        if (isRunningTestProfile()) {
            return;
        }

        try {
            documentEditorConsumer.attachQueueListener();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private boolean isRunningTestProfile() {
        final String TEST_PROFILE = "test";
        String currentProfile = ProfileManager.getActiveProfile();;
        return TEST_PROFILE.equalsIgnoreCase(currentProfile);
    }
}
