package dev.orion.api.endpoint.body;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@NoArgsConstructor
@AllArgsConstructor
public class AddUserToActivityRequestBody {
    @NotBlank
    public String userExternalId;
}
