package dev.orion.services.interfaces;

import java.io.InputStream;

public interface ActivitySetupService {
    public void createWorkflowByYaml(InputStream inputStream);
}
