package dev.orion.api.endpoint.body;

import lombok.Getter;
import lombok.Setter;
import org.eclipse.microprofile.openapi.annotations.media.Schema;

import java.util.List;
import java.util.UUID;

@Setter
@Getter
public class CreateActivityResponseBody {
    @Schema(required = true, example = "70f4bb61-4c6e-4aac-bb1d-56e4247e16b3")
    private UUID uuid;
    @Schema(required = true, example = "[70f4bb61-4c6e-4aac-bb1d-56e4247e16b3]")
    private List<UUID> groups;
}
