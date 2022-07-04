package dev.orion.broker.dto;


import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.UUIDSerializer;
import dev.orion.commom.constant.ActivityStage;
import dev.orion.entity.Activity;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
public class ActivityUpdateMessageDto {
    @JsonSerialize(using = UUIDSerializer.class)
    public UUID uuid;
    public Set<String> participants = new HashSet<>();
    public Boolean isActive;
    public ActivityStage activityStage;
    public Set<UserError> performErrors = Collections.synchronizedSet(new HashSet<>());

    private String createdAt = LocalDateTime.now().toString();
    @JsonSerialize(using = UUIDSerializer.class)
    private UUID messageKey;


    public void addError(UserError userError) {
        if (Objects.nonNull(userError)) {
            this.performErrors.add(userError);
        }
    }

    public ActivityUpdateMessageDto(Activity activity, UUID messageKey) {
        this.uuid = activity.uuid;
        this.participants = activity
                .participants
                .stream()
                .map(item -> item.externalId)
                .collect(Collectors.toSet());

        this.isActive = activity.isActive;
        this.messageKey = messageKey;
        this.activityStage = activity.getActualStage();
    }



    public static UserError.UserErrorBuilder getErrorBuilder() {
        return UserError.builder();
    }

    @Builder
    public static class UserError {
        public String externalUserId;
        public Integer code;
        public String message;
    }
}
