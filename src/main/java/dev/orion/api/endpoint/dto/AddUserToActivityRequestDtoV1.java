package dev.orion.api.endpoint.dto;

import javax.validation.constraints.NotBlank;

public class AddUserToActivityRequestDtoV1 {
    @NotBlank
    public String userExternalId;
}
