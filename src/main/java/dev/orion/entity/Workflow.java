package dev.orion.entity;

import dev.orion.commom.enums.ActivityStages;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.util.HashMap;
import java.util.Map;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class Workflow extends PanacheEntity {
    @Column(nullable = false)
    private String name;

    private String description;

    @MapKeyEnumerated(EnumType.STRING)
    @OneToMany(mappedBy = "hostWorkflow", cascade = CascadeType.ALL)
    private Map<ActivityStages, StepStage> stepStages = new HashMap<>();

    public void addStepStage(StepStage stepStage) {
        stepStages.put(stepStage.getStage(), stepStage);
    }
}
