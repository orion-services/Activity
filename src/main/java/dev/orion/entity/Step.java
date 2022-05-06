package dev.orion.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DiscriminatorColumn(discriminatorType = DiscriminatorType.STRING, name = "STEP_TYPE")
public abstract class Step extends PanacheEntity {
    @Column(nullable = false)
    private String type;
    private String description;

    public String getStepType() {
        return this.type;
    }

}
