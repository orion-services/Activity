package dev.orion.data.entity;

import dev.orion.util.enums.UserStatus;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

@Entity
public class User extends PanacheEntityBase {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "user_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    public UUID uuid;

    @Column(nullable = false)
    public String externalId;

    @Column(name = "user_status", nullable = false)
    public UserStatus status;

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

    public User() {
    }

    public User(String externalId) {
        this.externalId = externalId;
        this.status = UserStatus.AVAILABLE;
    }

    public static Optional<User> findUserByExternalId(String externalId) {
        return User.find("externalId", externalId).firstResultOptional();
    }
}
