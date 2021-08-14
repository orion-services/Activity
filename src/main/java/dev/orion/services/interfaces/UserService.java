package dev.orion.services.interfaces;

import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;

public interface UserService {
    public User getLocalUserByExternalId(String userExternalId);
    public UserCompleteDataDto getCompleteUserData(String userExternalId);
}
