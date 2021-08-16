package dev.orion.api.endpoint.v1.dto;

import javax.validation.constraints.NotBlank;

public class CreateActivityRequestDtoV1 {
    @NotBlank(message = "User may not be blank")
    private String userId;

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }
}
