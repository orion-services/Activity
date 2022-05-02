package dev.orion.entity;

import dev.orion.commom.enums.ActivityStages;
import dev.orion.entity.StepBase;
import dev.orion.entity.Workflow;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Setter
public class StepStage extends PanacheEntity {
    @Column(nullable = false)
    private ActivityStages stage;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    private List<StepBase> steps = new ArrayList<>();

    @ManyToOne
    private Workflow hostWorkflow;

    public void addStep(StepBase step) {
        steps.add(step);
    }
}
