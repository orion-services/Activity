package dev.orion.data.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

/**
 * WIP: HISTORY NOT IMPLEMENTED YET
 * **/

@SuppressWarnings("ALL")
@Entity
public class ActivityHistory extends PanacheEntity {

    @OneToMany(cascade = CascadeType.PERSIST)
    public Set<UserHistory> userHistoryList = new HashSet<>();

    @SuppressWarnings("unused")
    @OneToOne
            @JoinColumn(name = "activity_id")
    public Activity activity;

    @Column(name = "created_at")
    public LocalDateTime createdAt;

    @Column(name = "updated_at")
    public LocalDateTime updatedAt;

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
