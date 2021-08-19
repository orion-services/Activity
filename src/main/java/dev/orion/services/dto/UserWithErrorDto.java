package dev.orion.services.dto;

import dev.orion.data.entity.ErrorMessage;
import dev.orion.util.enums.UserError;
import java.util.HashSet;

import java.util.Set;

public class UserWithErrorDto {
    public Set<UserModelDto> userNotAdded = new HashSet<>();

    public void addUserWithProblem(UserCompleteDataDto user, ErrorMessage errorMessage) {
        var userModelDto = new UserModelDto(user.name, user.externalId, errorMessage.message, errorMessage.errorType);
        this.userNotAdded.add(userModelDto);
    }

    public void addUserWithProblem(String externalId, ErrorMessage errorMessage) {
        var userModelDto = new UserModelDto(externalId, errorMessage.message, errorMessage.errorType);
        this.userNotAdded.add(userModelDto);
    }

    private class UserModelDto {
        String name;
        String externalId;
        String reason;
        UserError errorType;

        public UserModelDto(String externalId, String reason, UserError errorType) {
            this.externalId = externalId;
            this.reason = reason;
            this.errorType = errorType;
        }

        public UserModelDto(String name, String externalId, String reason, UserError errorType) {
            this.name = name;
            this.externalId = externalId;
            this.reason = reason;
            this.errorType = errorType;
        }
    }
}
