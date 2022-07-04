package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
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
    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            name = "DOCUMENT_EDITORS",
            joinColumns = @JoinColumn(name = "document_id"),
            inverseJoinColumns = @JoinColumn(name = "participant_id")
    )
    private Set<User> participantsThatEdited = new LinkedHashSet<>();

    @OrderColumn
    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            name = "ASSIGNED_PARTICIPANTS",
            joinColumns = @JoinColumn(name = "document_id"),
            inverseJoinColumns = @JoinColumn(name = "participant_id")
    )
    private Set<User> participantsAssigned = new LinkedHashSet<>();

    @ManyToOne
    @JoinColumn(name = "groupActivity_id")
    @JsonBackReference
    private GroupActivity groupActivity;

    @ManyToOne()
    @JoinColumn(name = "user_starter_id")
    private User userStarter;

    private Integer rounds = 1;

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

    public static List<Document> findAllByUserIdAndGroup(String externalId, UUID groupId) {
        Map<String, Object> params = new HashMap<>();
        params.put("groupId", groupId);
        params.put("externalId", externalId);
        return find("from Document as doc " +
                "JOIN doc.participantsAssigned as pa " +
                "WHERE doc.groupActivity.uuid = :groupId AND pa.externalId = :externalId", params)
                .list();
    }

    public static Optional<Document> findByExternalId(String externalId) {
        return find("externalId", externalId).firstResultOptional();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Document document = (Document) o;
        return getExternalId().equals(document.getExternalId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getExternalId());
    }
}
