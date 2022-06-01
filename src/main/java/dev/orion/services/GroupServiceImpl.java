package dev.orion.services;

import dev.orion.commom.exception.UserInvalidOperationException;
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
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@ApplicationScoped
public class GroupServiceImpl implements GroupService {

    @LoggerName("GroupServiceImpl")
    Logger logger;

    @Inject
    DocumentService documentService;

//    @TODO add document to group
    @Override
    public GroupActivity createGroup(Activity activity) {
        val group = new GroupActivity();
        group.setActivityOwner(activity);
        group.setCapacity(activity.userList.size());
        activity.groupActivities.add(group);

        return group;
    }

    @Override
    public GroupActivity createGroup(Activity activity, Set<User> users) {
        val group = createGroup(activity);
        val document = documentService.createDocument(UUID.randomUUID(), "", users);

        group.addDocument(document);
        group.setCapacity(users.size());

        addUserListToGroup(group, users, document);

        return group;
    }

    @Override
    public void addUserToGroup(GroupActivity groupActivity, User user, Document document) {
        validateUserInsertionOnGroup(groupActivity, Set.of(user));

        addUserListToGroup(groupActivity, Set.of(user), document);
        groupActivity.persist();
    }

    private GroupActivity addUserListToGroup(GroupActivity groupActivity, Set<User> users, Document document) {
        validateUserInsertionOnGroup(groupActivity, users);

        users.forEach(user -> {
            groupActivity.addParticipant(user);
        });
        if (!groupActivity.getDocuments().contains(document)) {
            groupActivity.addDocument(document);
        }

        users.forEach(user -> {
            user.setGroupActivity(groupActivity);
            document.addParticipant(user);
        });

        return groupActivity;
    }

    private void validateUserInsertionOnGroup(GroupActivity groupActivity, Set<User> users) {
        userBelongsToSameActivityOfGroup(groupActivity, users);
        notInAnotherGroupValidation(users);
        groupCapacityValidation(groupActivity, users);
    }

    private void notInAnotherGroupValidation(Set<User> users) {
        val usersNotValid = users.stream().filter(user -> Objects.nonNull(user.getGroupActivity())).collect(Collectors.toSet());

        if (!usersNotValid.isEmpty()) {
            val errorMessage = MessageFormat.format("There are {0} users that are already in another group", users.size());
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void groupCapacityValidation(GroupActivity groupActivity, Set<User> users) {
        val usersToAdd = users
                .stream()
                .filter(user -> !groupActivity.getParticipants().contains(user))
                .collect(Collectors.toSet());

        val totalSizeWithNewUsers = usersToAdd.size() + groupActivity.getParticipants().size();
        if (groupActivity.getCapacity() < totalSizeWithNewUsers) {
            val errorMessage = MessageFormat.format("There are {0} users that can''t be placed on group {1} because its above the capacity", usersToAdd.size(), groupActivity.getUuid());
            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    private void userBelongsToSameActivityOfGroup(GroupActivity groupActivity, Set<User> users) {
        val usersNotValid = users
                .stream()
                .filter(user -> !groupActivity.getActivityOwner().userList.contains(user))
                .collect(Collectors.toSet());

        if (!usersNotValid.isEmpty()) {
            val errorMessage = MessageFormat
                    .format("There are {0} users that can''t be placed on group {1} because it not belongs to activity {2}",
                            users.size(), groupActivity.getUuid(), groupActivity.getActivityOwner().uuid);

            logger.error(errorMessage);
            throw new UserInvalidOperationException(errorMessage);
        }
    }

    @Override
    public void removeUserFromGroup(Activity activity, User user) {
        val groupActivity = user.getGroupActivity();
        groupActivity.removeParticipant(user);
        val documents = groupActivity.getDocuments();
        val documentsWIthUser = documents.stream().filter(document -> document.getParticipantsAssigned().contains(user));
        documentsWIthUser.forEach(document -> {document.removeParticipant(user);});

        emptyGroupCleaner(groupActivity);

        groupActivity.persist();
    }

    private void emptyGroupCleaner(GroupActivity groupActivity) {
        if (!groupActivity.getParticipants().isEmpty()) {
            return;
        }

        val activityOwner = groupActivity.getActivityOwner();
        activityOwner.getGroupActivities().remove(groupActivity);
        groupActivity.getDocuments().forEach(document -> { document.setGroupActivity(null); });

        GroupActivity.delete("uuid", groupActivity.getUuid());
    }

    @Override
    public void transferUserToGroup(Activity activity, User user, GroupActivity destinationGroupActivity) {
        throw new NotImplementedYetException();
    }

    @Override
    public void changeGroupCapacity(Activity activity, GroupActivity groupActivity, Integer newCapacity) {
        val totalGroupParticipants = groupActivity.getParticipants().size();
        if (totalGroupParticipants > newCapacity) {
            String message = MessageFormat.format("Capacity {0} is less than the number of group {1} participants ({2})", newCapacity, groupActivity.getUuid(), totalGroupParticipants);
            logger.error(message);
            throw new IllegalArgumentException(message);
        }

        val totalActivityParticipants = activity.getUserList().size();
        if (newCapacity > totalActivityParticipants) {
            String message = MessageFormat.format("Capacity {0} is more than the number of activity {1} participants ({2})", newCapacity, activity.getUuid(), totalActivityParticipants);
            logger.error(message);
            throw new IllegalArgumentException(message);
        }

        groupActivity.setCapacity(newCapacity);
        groupActivity.persist();
    }
}
