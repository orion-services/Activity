package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.ActivityGroup;
import dev.orion.entity.User;

import java.util.List;

public interface GroupService {
    ActivityGroup createGroup(Activity activity);
    ActivityGroup createGroup(Activity activity, List<User> activityGroup);
    void addUserToGroup(Activity activity, User user);
    void removeUserFromGroup(Activity activity, User user);
    void transferUserToGroup(Activity activity, User user, ActivityGroup destinationGroup);
    void changeGroupCapacity(Activity activity, ActivityGroup activityGroup);
}
