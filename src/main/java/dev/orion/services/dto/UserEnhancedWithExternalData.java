package dev.orion.services.dto;

import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.UserStatus;
import dev.orion.entity.User;


public class UserEnhancedWithExternalData extends UserClientResponse {
    public Long id;
    public UserStatus status;
    public User userEntity;

    public UserEnhancedWithExternalData(User userEntity, UserClientResponse userClientResponse) {
        super(userClientResponse.uuid, userClientResponse.name, userClientResponse.isActive, userClientResponse.role);
        this.id = userEntity.id;
        this.status = userEntity.status;
        this.userEntity = userEntity;
    }

    public User getUserEntity() {
        return userEntity;
    }
}
