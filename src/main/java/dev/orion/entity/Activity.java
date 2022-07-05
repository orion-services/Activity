package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.deser.std.UUIDDeserializer;
import com.fasterxml.jackson.databind.ser.std.UUIDSerializer;
import dev.orion.commom.constant.ActivityStage;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class Activity extends PanacheEntityBase {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "activity_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)", insertable = false)
    @JsonSerialize(using = UUIDSerializer.class)
    @JsonDeserialize(using = UUIDDeserializer.class)
    public UUID uuid;

    @OneToMany(mappedBy = "activityOwner", cascade = CascadeType.ALL)
    @JsonManagedReference
    public List<GroupActivity> groupActivities = new ArrayList<>();

    @OneToMany(mappedBy = "activity", cascade = CascadeType.PERSIST)
    @OrderColumn
    @JsonManagedReference
    public Set<User> participants = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    public Workflow workflow;

    @ManyToOne(optional = false, cascade = CascadeType.PERSIST)
    public User creator;

    public ActivityStage actualStage = ActivityStage.PRE;

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

    public static Optional<Activity> findByIdOptional(UUID id) {
        return Activity.findByIdOptional((Object) id);
    }

    public static Optional<Activity> findByCreator(String userExternalId) {
        return Activity.find("creator.externalId", userExternalId).firstResultOptional();
    }

    public void addParticipant(User user) {
        user.setActivity(this);
        participants.add(user);
    }

    public void remove(User user) {
        user.setActivity(null);
        participants.remove(user);
    }

    public void addGroup(GroupActivity groupActivity) {
        groupActivity.setActivityOwner(this);
        this.groupActivities.add(groupActivity);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Activity activity = (Activity) o;
        return uuid.equals(activity.uuid) && isActive.equals(activity.isActive);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uuid, isActive);
    }
}
