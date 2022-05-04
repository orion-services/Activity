package dev.orion.repository;

import dev.orion.entity.Activity;
import dev.orion.entity.Group;
import dev.orion.entity.User;
import io.quarkus.hibernate.orm.panache.PanacheRepository;

import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;

public class GroupRepository implements PanacheRepository<Group> {
    public void setNextParticipantByActivity(Activity activity, User user){
        Group group = find("activity", activity).firstResult();
        if (!group.getParticipants().contains(user)) {
            throw new NotFoundException(MessageFormat.format("User {0} not in group {1}", user.externalId, group.id));
        }

    }
}
