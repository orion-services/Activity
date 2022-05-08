package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import dev.orion.commom.enums.UserStatus;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Optional;

@Entity
@Setter
@Getter
@NoArgsConstructor
@JsonIgnoreProperties(value = {"id"})
public class User extends PanacheEntity {

    @Column(nullable = false, unique = true)
    public String externalId;

    @Column(name = "user_status", nullable = false)
    public UserStatus status;

    @ManyToOne(cascade = CascadeType.ALL)
    @JsonBackReference
    public Activity activity;

    LocalDateTime createdAt;

    LocalDateTime updatedAt;

    @ManyToOne(cascade = CascadeType.REFRESH)
    @JsonBackReference
    private ActivityGroup activityGroup;

    @PrePersist
    void createdAtUpdate() {
        this.createdAt = this.updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    void updatedAtUpdate() {
        this.updatedAt = LocalDateTime.now();
    }

    public User(String externalId) {
        this.externalId = externalId;
        this.status = UserStatus.AVAILABLE;
    }

    public static Optional<User> findUserByExternalId(String externalId) {
        return User.find("externalId", externalId).firstResultOptional();
    }
}
