package dev.orion.profiles;

import io.quarkus.test.junit.QuarkusTestProfile;

public class DisabledLifecycleEventsProfile implements QuarkusTestProfile {
    @Override
    public boolean disableApplicationLifecycleObservers() {
        return true;
    }
}