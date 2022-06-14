package dev.orion.api.endpoint.dto;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@NoArgsConstructor
@AllArgsConstructor
public class AddUserToActivityRequestDtoV1 {
    @NotBlank
    public String userExternalId;
}
