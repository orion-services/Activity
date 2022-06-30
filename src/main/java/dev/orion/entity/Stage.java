package dev.orion.entity;

import dev.orion.commom.constant.ActivityStages;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Entity
@Getter
@Setter
@RequiredArgsConstructor
@NoArgsConstructor
public class Stage extends PanacheEntity {
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    @NonNull
    private ActivityStages activityStage;

    @ManyToMany(cascade = CascadeType.ALL)
    @OrderColumn
    private List<Step> steps = new ArrayList<>();

    public void addStep(Step step) {
        steps.add(step);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Stage stage1 = (Stage) o;
        return activityStage == stage1.activityStage;
    }

    @Override
    public int hashCode() {
        return Objects.hash(activityStage);
    }
}
