package dev.orion.services.interfaces;

import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalData;

public interface UserService {
    public User getLocalUserByExternalId(String userExternalId);
    public UserEnhancedWithExternalData getCompleteUserData(String userExternalId);
}
