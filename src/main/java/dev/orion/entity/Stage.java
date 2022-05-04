package dev.orion.entity;

import dev.orion.commom.enums.ActivityStages;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Entity
@Getter
@Setter
public class Stage extends PanacheEntity {
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private ActivityStages stage;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Step> steps = new ArrayList<>();

    public void addStep(Step step) {
        steps.add(step);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Stage stage1 = (Stage) o;
        return stage == stage1.stage;
    }

    @Override
    public int hashCode() {
        return Objects.hash(stage);
    }
}
