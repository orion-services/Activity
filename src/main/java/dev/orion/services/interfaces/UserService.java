package dev.orion.services.interfaces;

import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;
import dev.orion.services.dto.UserCompleteDataListDto;

import java.util.Set;
import java.util.UUID;

public interface UserService {
    User getLocalUserByExternalId(String userExternalId);
    UserCompleteDataDto getCompleteUserData(String userExternalId);
    UserCompleteDataListDto getAllCompleteUserData(Set<String> userExternalIdList);
}
