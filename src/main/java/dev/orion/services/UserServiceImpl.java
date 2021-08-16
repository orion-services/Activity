package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;
import dev.orion.services.interfaces.UserService;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.Optional;

@ApplicationScoped
@Transactional
public class UserServiceImpl implements UserService {
    @Inject
    @RestClient
    UserClient userClient;

    @Override
    public User getLocalUserByExternalId(String userExternalId) throws RuntimeException {
        Optional<User> optUser = User.findUserByExternalId(userExternalId);
        if(optUser.isPresent()) {
            return  optUser.get();
        }

        User user = new User(userExternalId);
        user.persist();

        return user;
    }

    @Override
    public UserCompleteDataDto getCompleteUserData(String userExternalId) {
        Optional<User> optUserEntity = User.findUserByExternalId(userExternalId);
//     @TODO use this line to not remove mock   UserClientDto userClientDto = userClient.getUserByExternalId(userExternalId);
        UserClientDto userClientDto = mockUserCreation(userExternalId);
        User userEntity = optUserEntity.orElse(null);

        if(optUserEntity.isEmpty()) {
            User user = new User(userExternalId);
            user.persist();
            userEntity = user;
        }

        return new UserCompleteDataDto(userEntity, userClientDto);
    }

    private UserClientDto mockUserCreation(String externalUserId) {
        var userDto = new UserClientDto();
        userDto.externalId = externalUserId;
        userDto.name = "Toffu com Xuxu";
        userDto.isActive = true;

        return userDto;
    }
}
