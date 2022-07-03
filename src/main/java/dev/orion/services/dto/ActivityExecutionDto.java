package dev.orion.services.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.UUID;

@Data
@AllArgsConstructor
public class ActivityExecutionDto {
    UUID activityUUID;
    String documentExternalId;
    String userExternalId;
    String content;
}
