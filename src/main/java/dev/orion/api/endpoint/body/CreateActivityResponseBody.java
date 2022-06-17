package dev.orion.api.endpoint.body;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.UUID;

@Setter
@Getter
public class CreateActivityResponseBody {
    private UUID uuid;
    private List<UUID> groups;
}
