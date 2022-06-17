package dev.orion.api.endpoint.body;

import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
public class CreateActivityRequestBody {
    @NotBlank(message = "User may not be blank")
    private String userExternalId;

    @NotBlank(message = "Workflow may not be blank")
    private String workflowName;
}
