package dev.orion.services.dto;

import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.util.enums.UserStatus;


public class UserCompleteDataDto extends UserClientDto {
    public Long id;
    public UserStatus status;
    public User userEntity;

    public UserCompleteDataDto(User userEntity, UserClientDto userClientDto) {
        super(userClientDto.uuid, userClientDto.name, userClientDto.isActive);
        this.id = userEntity.id;
        this.status = userEntity.status;
        this.userEntity = userEntity;
    }

    public User getUserEntity() {
        return userEntity;
    }
}
