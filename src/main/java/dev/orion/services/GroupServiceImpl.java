package dev.orion.services;

import dev.orion.commom.exceptions.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;
import org.jboss.resteasy.spi.NotImplementedYetException;

import javax.enterprise.context.ApplicationScoped;
import java.text.MessageFormat;
import java.util.List;
import java.util.Objects;

@ApplicationScoped
public class GroupServiceImpl implements GroupService {

    @LoggerName("GroupServiceImpl")
    Logger logger;

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
        validateUserInsertionOnGroup(activity, group, user);

        group.addParticipant(user);
        user.setGroup(group);
    }

    private void validateUserInsertionOnGroup(Activity activity, Group group, User user) {
        notInAnotherGroupValidation(user);
        groupCapacityValidation(group, user);
        userBelongsToSameActivityOfGroup(group, user);
    }

    private void notInAnotherGroupValidation(User user) {
        if (!Objects.isNull(user.getGroup())) {
            val errorMessage = MessageFormat.format("The user {0} is already on another group", user.id);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void groupCapacityValidation(Group group, User user) {
        if (group.getCapacity() <= group.getParticipants().size() && !group.getParticipants().contains(user)) {
            val errorMessage = MessageFormat.format("The user {0} can't be placed on group {1} because its is full", user.id,group.id);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void userBelongsToSameActivityOfGroup(Group group, User user) {
        if (!group.getActivityOwner().userList.contains(user)) {
            val errorMessage = MessageFormat
                    .format("The user {0} can't be placed on group {1} because it not belongs to activity {2}",
                            user.id, group.id, group.getActivityOwner().uuid);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
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
