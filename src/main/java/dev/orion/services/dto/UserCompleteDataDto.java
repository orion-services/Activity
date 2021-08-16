package dev.orion.services.dto;

import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.util.enums.UserStatus;

import java.util.UUID;

public class UserCompleteDataDto extends UserClientDto {
    public UUID uuid;
    public UserStatus status;
    public User user;

    public UserCompleteDataDto(User user, UserClientDto userClientDto) {
        super(userClientDto.externalId, userClientDto.name, userClientDto.isActive);
        this.uuid = user.uuid;
        this.status = user.status;
        this.user = user;
    }

    public User getUser() {
        return user;
    }
}
