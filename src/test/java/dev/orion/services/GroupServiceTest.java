package dev.orion.services;

import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;
import dev.orion.fixture.UserFixture;
import dev.orion.services.interfaces.GroupService;
import io.quarkus.test.junit.QuarkusTest;
import lombok.val;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.BDDMockito;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;
import javax.transaction.Transactional;

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
        activity.groups.add(group);
        mainUser.setGroup(group);
        val spyGroup = Mockito.spy(group);

        groupService.addUserToGroup(activity, spyGroup, mainUser);
        Assertions.assertEquals(group.getParticipants().size(), 1);
        BDDMockito.then(spyGroup).should().addParticipant(mainUser);
    }

    @Test
    @DisplayName("Should throw exception when try to add an user that is already in another group")
    public void testAddUserInMoreThanOneGroup() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not let add participant if group is full")
    public void testAddToFullGroupValidation() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not let add participant if they not belongs to same activity that owns group")
    public void testAddUserThatNotBelongToActivityOwnerValidation() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should transfer an user between groups")
    public void testTransferUserBetweenGroups() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("Should not transfer an user if it's not present in activity")
    public void testTransferUserNotBelongingToActivityBetweenGroupsValidation() {
        throw new NotImplementedYetException();
    }

//    Capacity scenarios
    @Test
    @DisplayName("Should create a group with capacity same number of activity participants")
    public void testGroupCapacityAsSameActivityParticipantNumber() { throw new NotImplementedYetException();}

    @Test
    @DisplayName("Should create a group with same number of provided user list")
    public void testGroupCapacityAsSameProvidedUserList() { throw new NotImplementedYetException();}

    @Test
    @DisplayName("Should change capacity of group")
    public void testChangeGroupCapacity() { throw new NotImplementedYetException();}

    @Test
    @DisplayName("Should not change capacity when the new quantity is greater than number of participants")
    public void testChangeGroupCapacityBelowParticipantsNumberValidation() { throw new NotImplementedYetException();}

    //    Document scenarios
    @Test
    @DisplayName("Should add document and users whenever is possible")
    public void testAddDocumentAndUsers() {
        throw new NotImplementedYetException();
    }
}
