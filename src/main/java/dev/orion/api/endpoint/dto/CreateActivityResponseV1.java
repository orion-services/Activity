package dev.orion.api.endpoint.dto;

import java.util.UUID;

public class CreateActivityResponseV1 {
    private UUID uuid;

    public UUID getUuid() {
        return uuid;
    }

    public void setUuid(UUID uuid) {
        this.uuid = uuid;
    }
}
