package dev.orion.data.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

@Entity
public class Document extends PanacheEntity {

    @Column(nullable = false)
    public String content;

    @OneToOne(cascade = CascadeType.ALL, mappedBy = "document", optional = false)
    @JoinColumn(name = "activity_uuid")
    public Activity activity;

    @ManyToOne
    public User editedBy;

    @Column(name = "created_at")
    public LocalDateTime createdAt;

    @Column(name = "updated_at")
    public LocalDateTime updatedAt;

    @PrePersist
    void createdAtUpdate() {
        this.createdAt = this.updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    void updatedAtUpdate() {
        this.updatedAt = LocalDateTime.now();
    }

    public static Optional<Document> getDocumentByActivity(UUID activityUuid) {
        Optional<Activity> activity = Activity.findByIdOptional(activityUuid);
        if (activity.isPresent()) {
            return Optional.of(activity.get().document);
        }

        return Optional.empty();

    }
}
