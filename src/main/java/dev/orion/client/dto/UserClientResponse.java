package dev.orion.client.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserClientResponse {
    public String uuid;
    public String name;
    public Boolean isActive;

    public UserClientResponse() {
    }

    public UserClientResponse(String externalId, String name, Boolean isActive) {
        this.uuid = externalId;
        this.name = name;
        this.isActive = isActive;
    }
}
