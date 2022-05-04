package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.User;

// Control workflow of activity
public interface WorkflowManageService {
    public void apply(Activity activity, User performer);
}
