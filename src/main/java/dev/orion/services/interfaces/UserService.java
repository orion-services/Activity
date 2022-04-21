package dev.orion.services.interfaces;

import dev.orion.data.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalDataDto;

public interface UserService {
    public User getLocalUserByExternalId(String userExternalId);
    public UserEnhancedWithExternalDataDto getCompleteUserData(String userExternalId);
}
