package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import dev.orion.commom.constant.ActivityStages;
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

    @OneToMany(mappedBy = "activityOwner", cascade = CascadeType.ALL)
    @JsonManagedReference
    public List<GroupActivity> groupActivities = new ArrayList<>();

//    REMOVE
    @OneToOne
    public User userRound;

    @OneToMany(mappedBy = "activity", cascade = CascadeType.PERSIST)
    @OrderColumn
    @JsonManagedReference
    public Set<User> userList = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    public Workflow workflow;

    @ManyToOne(optional = false, cascade = CascadeType.PERSIST)
    public User createdBy;

    public ActivityStages actualStage = ActivityStages.PRE;

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

    public void addParticipant(User user) {
        user.setActivity(this);
        userList.add(user);
    }

    public void addGroup(GroupActivity groupActivity) {
        groupActivity.setActivityOwner(this);
        this.groupActivities.add(groupActivity);
    }
}
