package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import lombok.*;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.util.*;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(value = {"id"})
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class GroupActivity extends PanacheEntityBase {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "group_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    @EqualsAndHashCode.Include
    private UUID uuid;

    @OneToMany(mappedBy = "groupActivity", cascade = CascadeType.ALL)
    @OrderColumn
    @JsonManagedReference
    private Set<User> participants = new LinkedHashSet<>();

    @OrderColumn
    @OneToMany
    private Set<User> alreadyPlayedParticipants = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    @JsonBackReference
    private Activity activityOwner;

    @OneToMany
    private List<User> participantsRound;

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "groupActivity")
    @JsonManagedReference
    List<Document> documents = new ArrayList<>();

    public void addDocument(Document document) {
        document.setGroupActivity(this);
        documents.add(document);
    }

    public void addParticipantsRound(User participant) {
        participantsRound.add(participant);
    }
    public void addParticipant(User participant) {
        participant.setGroupActivity(this);
        participants.add(participant);
    }

    public void removeParticipant(User participant) {
        participant.setGroupActivity(null);
        participants.remove(participant);
    }

    private Integer capacity;
}
