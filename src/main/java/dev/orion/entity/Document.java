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
    @JoinTable(
            name = "DOCUMENT_EDITORS",
            joinColumns = @JoinColumn(name = "document_id"),
            inverseJoinColumns = @JoinColumn(name = "participant_id")
    )
    private Set<User> participantsThatEdited = new LinkedHashSet<>();

    @OrderColumn
    @ManyToMany
    @JoinTable(
            name = "ASSIGNED_PARTICIPANTS",
            joinColumns = @JoinColumn(name = "document_id"),
            inverseJoinColumns = @JoinColumn(name = "participant_id")
    )
    private Set<User> participantsAssigned = new LinkedHashSet<>();

    @ManyToOne
    @JoinColumn(name = "groupActivity_id")
    private GroupActivity groupActivity;

    public void assignParticipant(User user) {
        participantsAssigned.add(user);
    }
    public void removeParticipant(User user) {
        participantsAssigned.remove(user);
    }

    public void assignMultipleParticipants(Set<User> users) {
        participantsAssigned.addAll(users);
    }
    public void addParticipantThatEdited(User user) {
        participantsThatEdited.add(user);
    }
    public static List<Document> findAllByGroupActivity(UUID uuid) {
        return find("groupActivity_id", uuid).list();
    }

}
