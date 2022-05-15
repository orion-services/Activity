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
    @OneToMany
    private List<User> participantsThatEdited = new ArrayList<>();

    @OrderColumn
    @OneToMany
    private List<User> participantsAssigned = new ArrayList<>();


    @ManyToOne
    private ActivityGroup activityGroup;

}
