package dev.orion.entity;

import dev.orion.commom.constant.ActivityStage;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    @ElementCollection(targetClass = ActivityStage.class)
    @CollectionTable(name = "ALLOWED_STAGE_FOR_STEP")
    @Enumerated(EnumType.STRING)
    protected Set<ActivityStage> allowedStages = new HashSet<>(List.of(ActivityStage.values()));

    public String getStepType() {
        return this.type;
    }

}
