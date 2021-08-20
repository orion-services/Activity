package dev.orion.broker.dto;


import dev.orion.data.entity.Activity;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

public class ActivityUpdateMessageDto {
    public UUID uuid;
    public Set<String> participants = new HashSet<>();
    public String participantRound;
    public Boolean isActive;
    public Set<UserError> performErrors = new HashSet<>();

    public void addError(UserError userError) {
        this.performErrors.add(userError);
    }

    public ActivityUpdateMessageDto() {
    }

    public ActivityUpdateMessageDto(Activity activity) {
        this.uuid = activity.uuid;
        this.participants = activity
                .userList
                .stream()
                .map(item -> item.externalId)
                .collect(Collectors.toSet());

        this.participantRound = activity.userRound.externalId;
        this.isActive = activity.isActive;
    }



    public UserError getUserError() {
        return new UserError();
    }

    public ErrorType getErrorType() {
        return new ErrorType();
    }

    public static class UserError {
        public String externalUserId;
        public ErrorType type;
    }

    public static class ErrorType {
        public Integer code;
        public String message;
    }
}
