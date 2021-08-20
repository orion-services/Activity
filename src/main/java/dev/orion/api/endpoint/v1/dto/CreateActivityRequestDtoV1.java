package dev.orion.api.endpoint.v1.dto;

import javax.validation.constraints.NotBlank;

public class CreateActivityRequestDtoV1 {
    @NotBlank(message = "User may not be blank")
    private String userExternalId;

    public String getUserExternalId() {
        return userExternalId;
    }

    public void setUserExternalId(String userExternalId) {
        this.userExternalId = userExternalId;
    }
}
