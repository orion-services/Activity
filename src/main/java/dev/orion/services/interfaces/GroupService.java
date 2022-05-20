package dev.orion.services.interfaces;

import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;

import java.util.List;

public interface GroupService {
    GroupActivity createGroup(Activity activity);
    GroupActivity createGroup(Activity activity, List<User> users);
    void addUserToGroup(GroupActivity groupActivity, User user, Document document);
    void removeUserFromGroup(Activity activity, User user);
    void transferUserToGroup(Activity activity, User user, GroupActivity destinationGroupActivity);
    void changeGroupCapacity(Activity activity, GroupActivity groupActivity);
}
