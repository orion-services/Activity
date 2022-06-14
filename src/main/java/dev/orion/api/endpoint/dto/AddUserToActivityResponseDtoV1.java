package dev.orion.api.endpoint.dto;

import dev.orion.entity.Activity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class AddUserToActivityResponseDtoV1 {
    public UUID uuid;
    public Set<String> participants;
    public Boolean isActive;
    public LocalDateTime lastUpdated;
    public String createdBy;

    public AddUserToActivityResponseDtoV1(Activity activity) {
        this.uuid = activity.uuid;
        this.participants = activity
                .userList
                .stream()
                .map(item -> item.externalId)
                .collect(Collectors.toSet());
        this.isActive = activity.isActive;
        this.lastUpdated = activity.getUpdatedAt();
        this.createdBy = activity.createdBy.externalId;
    }
}
