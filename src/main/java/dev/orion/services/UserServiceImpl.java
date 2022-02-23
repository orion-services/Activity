package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalDataDto;
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
    public UserEnhancedWithExternalDataDto getCompleteUserData(String userExternalId) {
        Optional<User> optUserEntity = User.findUserByExternalId(userExternalId);

        UserClientDto userClientDto = userClient.getUserByExternalId(userExternalId);
        User userEntity = optUserEntity.orElse(null);

        if(optUserEntity.isEmpty()) {
            User user = new User(userExternalId);
            user.persist();
            userEntity = user;
        }

        return new UserEnhancedWithExternalDataDto(userEntity, userClientDto);
    }
}
