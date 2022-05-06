package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.LinkedHashSet;
import java.util.Set;

@Entity
@Getter
@Setter
public class ActivityGroup extends PanacheEntity {

    @OneToMany(mappedBy = "activityGroup", cascade = CascadeType.ALL)
    @OrderColumn
    @JsonManagedReference
    private Set<User> participants = new LinkedHashSet<>();

    @OrderColumn
    @OneToMany
    private Set<User> alreadyPlayedParticipants = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    @JsonBackReference
    private Activity activityOwner;

    @OneToOne
    private User participantRound;
}
