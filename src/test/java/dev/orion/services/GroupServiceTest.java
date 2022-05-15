package dev.orion.services;

import dev.orion.services.interfaces.GroupService;
import io.quarkus.test.junit.QuarkusTest;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;

import javax.inject.Inject;

@QuarkusTest
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
        throw new NotImplementedYetException();
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
    public void testAddUserThatNotBelongToActivityOwner() {
        throw new NotImplementedYetException();
    }

//    Document scenarios
    @Test
    @DisplayName("Should add document and users whenever is possible")
    public void testAddDocumentAndUsers() {
        throw new NotImplementedYetException();
    }
}
