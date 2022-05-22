package dev.orion.services.dto;

import dev.orion.client.dto.UserClientResponse;
import dev.orion.entity.User;
import dev.orion.commom.enums.UserStatus;


public class UserEnhancedWithExternalDataResponse extends UserClientResponse {
    public Long id;
    public UserStatus status;
    public User userEntity;

    public UserEnhancedWithExternalDataResponse(User userEntity, UserClientResponse userClientResponse) {
        super(userClientResponse.uuid, userClientResponse.name, userClientResponse.isActive);
        this.id = userEntity.id;
        this.status = userEntity.status;
        this.userEntity = userEntity;
    }

    public User getUserEntity() {
        return userEntity;
    }
}
