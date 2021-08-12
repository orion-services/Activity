package dev.orion.client.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserClientDto {
    public String externalId;
    public String name;
    public Boolean isActive;
}
