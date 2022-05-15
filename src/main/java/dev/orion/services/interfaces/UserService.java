package dev.orion.services.interfaces;

import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalDataResponse;

public interface UserService {
    public User getLocalUserByExternalId(String userExternalId);
    public UserEnhancedWithExternalDataResponse getCompleteUserData(String userExternalId);
}
