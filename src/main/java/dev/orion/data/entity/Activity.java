package dev.orion.data.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.*;

@Entity
public class Activity extends PanacheEntityBase {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "activity_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    public UUID uuid;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JsonIgnore
    public Document document;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    public List<User> userList = new ArrayList<>();

    @ManyToOne
    @JsonInclude
    public User userRound;

    @ManyToOne(optional = false)
    public User createdBy;

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
