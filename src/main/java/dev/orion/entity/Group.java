package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(value = {"id"})
public class Group extends PanacheEntity {

    @OneToMany(mappedBy = "group", cascade = CascadeType.ALL)
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

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "group")
    List<Document> documents = new ArrayList<>();

    public void addDocument(Document document) {
        documents.add(document);
    }

    public void addParticipantsRound(User participant) {
        participantsRound.add(participant);
    }
    public void addParticipant(User participant) {participants.add(participant);}

    private Integer capacity;
}
