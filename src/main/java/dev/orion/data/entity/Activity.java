package dev.orion.data.entity;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.*;

@Entity
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Activity extends PanacheEntityBase {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @GenericGenerator(name = "activity_uuid", strategy = "uuid")
    @Column(columnDefinition = "BINARY(16)")
    public UUID uuid;

    @OneToOne(cascade = CascadeType.PERSIST)
    public Document document;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    public List<User> userList = new ArrayList<>();

    @ManyToOne
    public User userRound;

    @Column(nullable = false)
    Boolean isActive = true;

    LocalDateTime createdAt;

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
