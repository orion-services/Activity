package dev.orion.services.dto;

import java.util.HashSet;
import java.util.Set;

public class UserCompleteDataListDto {
    public Set<UserCompleteDataDto> userList = new HashSet<>();
    public Set<String> usersNotFound = new HashSet<>();
}
