package dev.orion.services.interfaces;

import dev.orion.services.dto.UserEnhancedWithExternalData;

public interface UserService {
    public UserEnhancedWithExternalData getCompleteUserData(String userExternalId);
    public Long connectUser(String userExternalId);
}
