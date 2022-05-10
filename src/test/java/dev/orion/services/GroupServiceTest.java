package dev.orion.services;

import io.quarkus.test.junit.QuarkusTest;
import org.jboss.resteasy.spi.NotImplementedYetException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;

@QuarkusTest
public class GroupServiceTest {

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    @DisplayName("It should create a empty group by activity")
    public void testEmptyGroupCreation() {
        throw new NotImplementedYetException();
    }

    @Test
    @DisplayName("")
    public void test() {
        throw new NotImplementedYetException();
    }
}
