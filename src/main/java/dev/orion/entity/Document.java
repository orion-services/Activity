package dev.orion.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.Setter;
import org.jose4j.jwk.Use;

import javax.persistence.*;
import java.util.*;

@Entity
@Getter
@Setter
public class Document extends PanacheEntity {
    @Column(nullable = false, unique = true)
    private String externalId;

    @OrderColumn
    @ManyToMany
    private Set<User> participantsThatEdited = new LinkedHashSet<>();

    @OrderColumn
    @ManyToMany
    @JoinTable(name = "ACTIVE_PARTICIPANTS")
    private Set<User> participantsAssigned = new LinkedHashSet<>();

    @ManyToOne
    private GroupActivity groupActivity;

    public void assignParticipant(User user) {
        participantsAssigned.add(user);
    }

    public void assignMultipleParticipants(Set<User> users) {
        participantsAssigned.addAll(users);
    }

    public void addParticipantThatEdited(User user) {
        participantsThatEdited.add(user);
    }

}
