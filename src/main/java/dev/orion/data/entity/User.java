package dev.orion.data.entity;

import dev.orion.util.enums.UserStatus;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
public class User extends PanacheEntityBase {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "user_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    public UUID uuid;

    @Column(name = "user_status", nullable = false)
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
