package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.services.interfaces.UserService;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.util.Optional;
import java.util.UUID;

@ApplicationScoped
public class UserServiceImpl implements UserService {
    @Inject
    @RestClient
    UserClient userClient;

    @Override
    public User getUserByExternalId(String userExternalId) throws RuntimeException {
        Optional<User> optUser = User.findUserByExternalId(userExternalId);
        if(optUser.isPresent()) {
            return  optUser.get();
        }
        var clientUser = this.mockUserCreation();
        if (clientUser.isActive.equals(Boolean.FALSE)) {
            throw new RuntimeException("User must be active");
        }

        User user = new User(userExternalId);
        user.persist();

        return user;
    }

    private UserClientDto mockUserCreation() {
        var userDto = new UserClientDto();
        userDto.externalId = UUID.randomUUID().toString();
        userDto.name = "Toffu com Xuxu";
        userDto.isActive = true;

        return userDto;
    }
}
