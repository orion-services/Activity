package dev.orion.repository;

import dev.orion.entity.Activity;
import dev.orion.entity.GroupActivity;
import dev.orion.entity.User;
import io.quarkus.hibernate.orm.panache.PanacheRepository;

import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;

public class GroupRepository implements PanacheRepository<GroupActivity> {
    public void setNextParticipantByActivity(Activity activity, User user){
        GroupActivity groupActivity = find("activity", activity).firstResult();
        if (!groupActivity.getParticipants().contains(user)) {
            throw new NotFoundException(MessageFormat.format("User {0} not in group {1}", user.externalId, groupActivity.getUuid()));
        }

    }
}
