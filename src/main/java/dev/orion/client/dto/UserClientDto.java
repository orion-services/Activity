package dev.orion.client.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserClientDto {
    public String uuid;
    public String name;
    public String email;
    public Boolean isActive;

    public UserClientDto() {
    }

    public UserClientDto(String externalId, String name, Boolean isActive, String email) {
        this.uuid = externalId;
        this.name = name;
        this.isActive = isActive;
        this.email = email;
    }
}
