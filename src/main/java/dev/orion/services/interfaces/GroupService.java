package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.ActivityGroup;
import dev.orion.entity.User;

public interface GroupService {
    public ActivityGroup createGroup(Activity activity);
    public ActivityGroup createGroup(Activity activity, ActivityGroup activityGroup);
    public void addUserToGroup(Activity activity, User user);
    public void removeUserFromGroup(Activity activity, User user);
    public void transferUserToGroup(Activity activity, User user, ActivityGroup destinationGroup);
    public void changeGroupCapacity(Activity activity);
}
