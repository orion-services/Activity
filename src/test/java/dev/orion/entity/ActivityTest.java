package dev.orion.entity;

import dev.orion.commom.enums.UserStatus;
import dev.orion.fixture.UserFixture;
import dev.orion.commom.enums.CircularStepFlowDirectionTypes;
import io.quarkus.test.junit.QuarkusTest;
import org.junit.jupiter.api.Test;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;

@QuarkusTest
@Transactional
public class ActivityTest {

    @Test
    public void shouldMaintainUserInsertionOrder() {
        CircularStepFlowDirectionTypes.valueOf("FROM_BEGIN_TO_END");
        Activity activity = new Activity();
        activity.isActive = true;
        List<User> users = populateDbWithUsers(12);
        activity.createdBy = users.get(0);

//        Prevent share the same instance of user list
        activity.userList.addAll(new ArrayList<>(users));
        activity.persist();


        Activity testingThis = Activity.findById(activity.uuid);
        AtomicInteger counter = new AtomicInteger();
        assertThat(testingThis.userList, contains(users.toArray()));
    }


    private List<User> populateDbWithUsers(Integer quantity) {
        Integer counter = quantity;
        List<User> userList = new ArrayList<>();
        do {
            User user = UserFixture.generateUser();
//            clean value to be inserted by Panache
            user.id = null;
            user.status = UserStatus.CONNECTED;
            user.persist();
            userList.add(user);
        } while (--counter > 0);

        return userList;
    }
}
