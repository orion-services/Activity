package dev.orion.entity;

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
public class Group extends PanacheEntity {

    @OneToMany(mappedBy = "group", cascade = CascadeType.ALL)
    @OrderColumn
    @JsonManagedReference
    private Set<User> participants = new LinkedHashSet<>();

    @OneToMany
    @Column(nullable = false)
    @JsonManagedReference
    private Activity activityOwner;

    @OneToOne
    private User participantRound;
}
