package dev.orion.entity.step_type;

import dev.orion.entity.Step;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Entity
@Getter
@Setter
@DiscriminatorValue("UNORDERED_CIRCLE_OF_WRITES")
@NoArgsConstructor
public class UnorderedCircleOfWriters extends Step {
    final private String type = "UNORDERED_CIRCLE_OF_WRITES";
    final private String description = "Following the Circle of Learners, this step ir circular but with flexible order";

    private Integer rounds = 1;
}
