package dev.orion.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import dev.orion.commom.constant.UserStatus;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Objects;
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
    @Enumerated(EnumType.STRING)
    public UserStatus status;

    @ManyToOne(cascade = CascadeType.ALL)
    @JsonBackReference
    public Activity activity;

    LocalDateTime createdAt;

    LocalDateTime updatedAt;

    @ManyToOne(cascade = CascadeType.REFRESH)
    @JsonBackReference
    private GroupActivity groupActivity;

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
        this.status = UserStatus.DISCONNECTED;
    }

    public static Optional<User> findUserByExternalId(String externalId) {
        return User.find("externalId", externalId).firstResultOptional();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return getExternalId().equals(user.getExternalId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getExternalId());
    }
}
