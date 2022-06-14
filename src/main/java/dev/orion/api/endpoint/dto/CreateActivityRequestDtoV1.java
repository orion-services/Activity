package dev.orion.api.endpoint.dto;

import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
public class CreateActivityRequestDtoV1 {
    @NotBlank(message = "User may not be blank")
    private String userExternalId;

    @NotBlank(message = "Workflow may not be blank")
    private String workflowName;
}
