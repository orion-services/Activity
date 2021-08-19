package dev.orion.client.dto;

import java.util.Set;

public class FindUserListResponseDto {
    public Set<UserClientDto> user;
    public Set<String> usersNotFound;
}
