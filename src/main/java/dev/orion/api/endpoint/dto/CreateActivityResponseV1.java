package dev.orion.api.endpoint.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.UUID;

@Setter
@Getter
public class CreateActivityResponseV1 {
    private UUID uuid;
    private List<UUID> groups;
}
