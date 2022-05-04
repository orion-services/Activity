package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import dev.orion.commom.enums.ActivityStages;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Getter
@Setter
public class Activity extends PanacheEntityBase {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "activity_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    @Setter(AccessLevel.NONE)
    public UUID uuid;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JsonIgnore
    public Document document;

    @OneToMany(mappedBy = "activityOwner")
    @JsonManagedReference
    public List<Group> groups = new ArrayList<>();

//    REMOVE
    @OneToOne
    public User userRound;

    @OneToMany(mappedBy = "activity", cascade = CascadeType.ALL)
    @OrderColumn
    @JsonManagedReference
    public Set<User> userList = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    public Workflow workflow;

    @ManyToOne(optional = false)
    public User createdBy;

    public ActivityStages activityStage = ActivityStages.PRE;

    @Column(nullable = false)
    public Boolean isActive = true;

    LocalDateTime createdAt;

    LocalDateTime updatedAt;

    @PrePersist
    void createdAtUpdate() {
        this.createdAt = this.updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    void updatedAtUpdate() {
        this.updatedAt = LocalDateTime.now();
    }
}
