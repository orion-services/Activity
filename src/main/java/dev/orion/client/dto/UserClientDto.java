package dev.orion.client.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserClientDto {
    public String uuid;
    public String name;
    public Boolean isActive;

    public UserClientDto() {
    }

    public UserClientDto(String externalId, String name, Boolean isActive) {
        this.uuid = externalId;
        this.name = name;
        this.isActive = isActive;
    }
}
