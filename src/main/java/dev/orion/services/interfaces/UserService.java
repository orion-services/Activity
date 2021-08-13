package dev.orion.services.interfaces;

import dev.orion.data.entity.User;

public interface UserService {
    public User getUserByExternalId(String userExternalId);
}
