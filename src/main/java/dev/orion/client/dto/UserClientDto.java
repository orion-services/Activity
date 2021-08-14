package dev.orion.client.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserClientDto {
    public String externalId;
    public String name;
    public Boolean isActive;

    public UserClientDto() {
    }

    public UserClientDto(String externalId, String name, Boolean isActive) {
        this.externalId = externalId;
        this.name = name;
        this.isActive = isActive;
    }
}
