package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;

import java.util.List;

public interface GroupService {
    Group createGroup(Activity activity);
    Group createGroup(Activity activity, List<User> users);
    void addUserToGroup(Activity activity, Group group,User user);
    void removeUserFromGroup(Activity activity, User user);
    void transferUserToGroup(Activity activity, User user, Group destinationGroup);
    void changeGroupCapacity(Activity activity, Group group);
}
