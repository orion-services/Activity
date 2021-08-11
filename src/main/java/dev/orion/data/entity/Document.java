package dev.orion.data.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
public class Document extends PanacheEntity {

    @Column(nullable = false)
    public String content;

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
