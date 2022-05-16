package dev.orion.services;

import dev.orion.commom.exceptions.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;
import dev.orion.fixture.UserFixture;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.test.junit.QuarkusTest;
import lombok.val;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.text.MessageFormat;

@QuarkusTest
@Transactional
public class GroupServiceTest {

    @Inject
    GroupService groupService;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    //    User scenarios tests
    @Test
    @DisplayName("Should add user to the group")
    public void testAddToTheGroup() {
        val activity = new Activity();
        val mainUser = UserFixture.generateUser();
        activity.userList.add(mainUser);

        val group = groupService.createGroup(activity);
        val spyGroup = Mockito.spy(group);

        activity.groups.add(group);

        groupService.addUserToGroup(activity, spyGroup, mainUser);
        Assertions.assertEquals(group.getParticipants().size(), 1);
        Assertions.assertEquals(mainUser.getGroup(), spyGroup);
        BDDMockito.then(spyGroup).should().addParticipant(mainUser);
    }

    @Test
    @DisplayName("Should throw exception when try to add an user that is already in another group")
    public void testAddUserInMoreThanOneGroup() {
        val activity = new Activity();
        val mainUser = UserFixture.generateUser();
        activity.userList.add(mainUser);

        val groupOrigin = groupService.createGroup(activity);
        val groupDestination = groupService.createGroup(activity);

        activity.groups.add(groupOrigin);
        activity.groups.add(groupDestination);

        groupService.addUserToGroup(activity, groupOrigin, mainUser);
        String errorMessage = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(activity, groupDestination, mainUser);
        }).getMessage();
        Assertions.assertEquals(MessageFormat.format("The user {0} is already on another group", mainUser.id), errorMessage);
        Assertions.assertNotEquals(mainUser.getGroup(), groupDestination);
    }

    @Test
    @DisplayName("Should not let add participant if group is full")
    public void testAddToFullGroupValidation() {
        val activity = new Activity();
        val mainUser = UserFixture.generateUser();
        activity.userList.add(mainUser);

        val group = groupService.createGroup(activity);
        groupService.addUserToGroup(activity, group, mainUser);
        val secondUser = UserFixture.generateUser();

        activity.groups.add(group);

        String message = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(activity, group, secondUser);
        }).getMessage();

        String expectedErrorMessage = MessageFormat.format("The user {0} can't be placed on group {1} because its is full", secondUser.id, group.id);
        Assertions.assertEquals(expectedErrorMessage, message);
    }

    @Test
    @DisplayName("Should not let add participant if they not belongs to same activity that owns group")
    public void testAddUserThatNotBelongToActivityOwnerValidation() {
        val activity = new Activity();
        val mainUser = UserFixture.generateUser();

        val group = groupService.createGroup(activity);
        group.setCapacity(2);

        activity.groups.add(group);

        String message = Assertions.assertThrows(UserInvalidOperationException.class, () -> {
            groupService.addUserToGroup(activity, group, mainUser);
        }).getMessage();
        String expectedErrorMessage = MessageFormat
                .format("The user {0} can't be placed on group {1} because it not belongs to activity {2}",
                        mainUser.id, group.id, activity.uuid);
        Assertions.assertEquals(expectedErrorMessage, message);
    }

    @Test
    @DisplayName("Should transfer an user between groups")
    @Disabled
    public void testTransferUserBetweenGroups() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not transfer an user if it's not present in activity")
    @Disabled
    public void testTransferUserNotBelongingToActivityBetweenGroupsValidation() {
        throw new NotImplementedYetException();
    }

    //    Capacity scenarios
    @Test
    @DisplayName("Should create a group with capacity same number of activity participants")
    @Disabled
    public void testGroupCapacityAsSameActivityParticipantNumber() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should create a group with same number of provided user list")
    @Disabled
    public void testGroupCapacityAsSameProvidedUserList() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should change capacity of group")
    @Disabled
    public void testChangeGroupCapacity() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not change capacity when the new quantity is greater than number of participants")
    @Disabled
    public void testChangeGroupCapacityBelowParticipantsNumberValidation() {
        throw new NotImplementedYetException();
    }

    //    Document scenarios
    @Test
    @DisplayName("Should add document and users whenever is possible")
    @Disabled
    public void testAddDocumentAndUsers() {
        throw new NotImplementedYetException();
    }
}
