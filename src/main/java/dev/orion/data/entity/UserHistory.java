package dev.orion.data.entity;

import dev.orion.data.composite_key.UserHistoryCompositeKey;
import dev.orion.util.enums.UserStatus;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;

import javax.persistence.*;
import java.time.LocalDateTime;

/**
 * WIP: HISTORY NOT IMPLEMENTED YET
 * **/
@Entity
public class UserHistory extends PanacheEntityBase {
    @EmbeddedId
    public UserHistoryCompositeKey userHistoryId;

    @ManyToOne
            @MapsId("userUuid")
            @JoinColumn(name = "id")
    public User user;

    @ManyToOne(cascade = CascadeType.PERSIST)
        @MapsId("activityId")
    public ActivityHistory activityHistory;

    public UserStatus status;

    @Column(name = "created_at")
    LocalDateTime createdAt;

    @Column(name = "updated_at")
    LocalDateTime updatedAt;

    @Version
    public int version;

    @PrePersist
    void createdAtUpdate() {
        this.createdAt = this.updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    void updatedAtUpdate() {
        this.updatedAt = LocalDateTime.now();
    }

}


