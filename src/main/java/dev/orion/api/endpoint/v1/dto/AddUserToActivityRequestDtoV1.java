package dev.orion.api.endpoint.v1.dto;

import javax.validation.constraints.NotBlank;

public class AddUserToActivityRequestDtoV1 {
    @NotBlank
    public String userExternalId;
}
