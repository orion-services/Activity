package dev.orion.util.setup;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class WorkflowFormat {
    private String name;
    private StepsStageMapFormat steps;
}
