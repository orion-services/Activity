package dev.orion.entity.step_type;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.entity.Step;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import java.util.Set;

@Entity
@Getter
@Setter
@DiscriminatorValue("UNORDERED_CIRCLE_OF_WRITES")
public class UnorderedCircleOfWriters extends Step {
    @Getter(AccessLevel.NONE)
    final private String type = "UNORDERED_CIRCLE_OF_WRITES";
    final private String description = "Following the Circle of Learners, this step ir circular but with flexible order";

    private Integer rounds = 1;

    public UnorderedCircleOfWriters() {
        allowedStages = Set.of(ActivityStage.DURING);
    }
}
