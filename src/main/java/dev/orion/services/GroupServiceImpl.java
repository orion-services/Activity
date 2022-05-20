package dev.orion.services;

import dev.orion.commom.exceptions.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Document;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import dev.orion.services.interfaces.DocumentService;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.jboss.logging.Logger;
import org.jboss.resteasy.spi.NotImplementedYetException;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.List;
import java.util.Objects;

@ApplicationScoped
public class GroupServiceImpl implements GroupService {

    @LoggerName("GroupServiceImpl")
    Logger logger;

    @Inject
    DocumentService documentService;

    @Override
    public GroupActivity createGroup(Activity activity) {
        val group = new GroupActivity();
        group.setActivityOwner(activity);
        group.setCapacity(activity.userList.size());
        activity.groupActivities.add(group);

        return group;
    }

    @Override
    public GroupActivity createGroup(Activity activity, List<User> users) {
        throw new NotImplementedYetException();
    }

    @Override
    public void addUserToGroup(GroupActivity groupActivity, User user, Document document) {
        validateUserInsertionOnGroup(groupActivity, user);

        groupActivity.addParticipant(user);
        user.setGroupActivity(groupActivity);

        if (!groupActivity.getDocuments().contains(document)) {
            groupActivity.addDocument(document);
        }

        document.assignParticipant(user);
        groupActivity.persist();
    }

    private void validateUserInsertionOnGroup(GroupActivity groupActivity, User user) {
        notInAnotherGroupValidation(user);
        groupCapacityValidation(groupActivity, user);
        userBelongsToSameActivityOfGroup(groupActivity, user);
    }

    private void notInAnotherGroupValidation(User user) {
        if (!Objects.isNull(user.getGroupActivity())) {
            val errorMessage = MessageFormat.format("The user {0} is already on another group", user.id);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void groupCapacityValidation(GroupActivity groupActivity, User user) {
        if (groupActivity.getCapacity() <= groupActivity.getParticipants().size() && !groupActivity.getParticipants().contains(user)) {
            val errorMessage = MessageFormat.format("The user {0} can't be placed on group {1} because its is full", user.id, groupActivity.id);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void userBelongsToSameActivityOfGroup(GroupActivity groupActivity, User user) {
        if (!groupActivity.getActivityOwner().userList.contains(user)) {
            val errorMessage = MessageFormat
                    .format("The user {0} can't be placed on group {1} because it not belongs to activity {2}",
                            user.id, groupActivity.id, groupActivity.getActivityOwner().uuid);
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    @Override
    public void removeUserFromGroup(Activity activity, User user) {
        throw new NotImplementedYetException();
    }

    @Override
    public void transferUserToGroup(Activity activity, User user, GroupActivity destinationGroupActivity) {
        throw new NotImplementedYetException();
    }

    @Override
    public void changeGroupCapacity(Activity activity, GroupActivity groupActivity) {
        throw new NotImplementedYetException();
    }
}
