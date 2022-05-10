package dev.orion.services;

import dev.orion.entity.Activity;
import dev.orion.entity.ActivityGroup;
import dev.orion.entity.User;
import dev.orion.services.interfaces.GroupService;
import org.jboss.resteasy.spi.NotImplementedYetException;

import java.util.List;

public class GroupServiceImpl implements GroupService {
    @Override
    public ActivityGroup createGroup(Activity activity) {
        throw new NotImplementedYetException();
    }

    @Override
    public ActivityGroup createGroup(Activity activity, List<User> activityGroup) {
        throw new NotImplementedYetException();
    }

    @Override
    public void addUserToGroup(Activity activity, User user) {
        throw new NotImplementedYetException();
    }

    @Override
    public void removeUserFromGroup(Activity activity, User user) {
        throw new NotImplementedYetException();
    }

    @Override
    public void transferUserToGroup(Activity activity, User user, ActivityGroup destinationGroup) {
        throw new NotImplementedYetException();
    }

    @Override
    public void changeGroupCapacity(Activity activity, ActivityGroup activityGroup) {
        throw new NotImplementedYetException();
    }
}
