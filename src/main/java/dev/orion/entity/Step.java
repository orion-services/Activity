package dev.orion.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DiscriminatorColumn(discriminatorType = DiscriminatorType.STRING, name = "STEP_TYPE")
public abstract class Step extends PanacheEntity {
    @Column(nullable = false, updatable = false)
    private String type;
    @Column(updatable = false)
    private String description;

    public String getStepType() {
        return this.type;
    }

}
