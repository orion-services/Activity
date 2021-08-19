package dev.orion.data.entity;

import dev.orion.util.enums.UserError;
import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.Column;
import javax.persistence.Entity;

@Entity
public class ErrorMessage extends PanacheEntity {
    @Column(unique = true)
    public UserError errorType;

    public String message;
}
