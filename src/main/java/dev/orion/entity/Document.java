package dev.orion.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.Setter;

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

    @ManyToOne()
    @JoinColumn(name = "user_creator_id")
    private User userCreator;

    public void addParticipant(User user) {
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

    public static Optional<Document> findByUserIdAndGroup(String externalId, UUID groupId) {
        Map<String, Object> params = new HashMap<>();
        params.put("groupId", groupId);
        params.put("externalId", externalId);
        return find("from Document as doc " +
                "JOIN doc.participantsAssigned as pa " +
                "WHERE doc.groupActivity.uuid = :groupId AND pa.externalId = :externalId", params)
                .firstResultOptional();
    }
}
