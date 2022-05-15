package dev.orion.services;

import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;
import dev.orion.services.interfaces.GroupService;
import lombok.val;
import org.jboss.resteasy.spi.NotImplementedYetException;

import javax.enterprise.context.ApplicationScoped;
import java.util.List;

@ApplicationScoped
public class GroupServiceImpl implements GroupService {

    @Override
    public Group createGroup(Activity activity) {
        val group = new Group();
        group.setActivityOwner(activity);
        group.setCapacity(activity.userList.size());
        activity.groups.add(group);

        return group;
    }

    @Override
    public Group createGroup(Activity activity, List<User> users) {
        throw new NotImplementedYetException();
    }

    @Override
    public void addUserToGroup(Activity activity, Group group, User user) {
        group.addParticipant(user);
    }

    @Override
    public void removeUserFromGroup(Activity activity, User user) {
        throw new NotImplementedYetException();
    }

    @Override
    public void transferUserToGroup(Activity activity, User user, Group destinationGroup) {
        throw new NotImplementedYetException();
    }

    @Override
    public void changeGroupCapacity(Activity activity, Group group) {
        throw new NotImplementedYetException();
    }
}
