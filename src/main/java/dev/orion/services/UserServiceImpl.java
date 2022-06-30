package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.constant.UserStatus;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import dev.orion.services.interfaces.UserService;
import io.quarkus.arc.log.LoggerName;
import lombok.val;
import org.eclipse.microprofile.config.ConfigProvider;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.jboss.logging.Logger;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import java.text.MessageFormat;
import java.util.Optional;

@ApplicationScoped
@Transactional
public class UserServiceImpl implements UserService {
    @Inject
    @RestClient
    UserClient userClient;

    @LoggerName("UserServiceImpl")
    Logger logger;


    @Override
    public UserEnhancedWithExternalData getCompleteUserData(String userExternalId) {
        Optional<User> optUserEntity = User.findUserByExternalId(userExternalId);

        UserClientResponse userClientResponse = Optional.ofNullable(userClient.getUserByExternalId(userExternalId)).orElseThrow(() -> {
            val userClientURL = ConfigProvider.getConfig().getValue("api.user-service.client/mp-rest/url", String.class);
            val errorMessage = MessageFormat.format("User {0} not found in user service in {1}", userExternalId, userClientURL);
            throw new NotFoundException(errorMessage);
        });
        User userEntity = optUserEntity.orElse(null);

        if(optUserEntity.isEmpty()) {
            User user = new User(userExternalId);
            user.persist();
            userEntity = user;
        }

        return new UserEnhancedWithExternalData(userEntity, userClientResponse);
    }

    @Override
    public Long connectUser(String userExternalId) {
        val userResponse = getCompleteUserData(userExternalId);
        val user = userResponse.getUserEntity();
        if (user.activity == null) {
            throw new UserInvalidOperationException(MessageFormat.format("The user {0} must be in activity to be connected", userExternalId));
        }

        user.setStatus(UserStatus.CONNECTED);
        logger.info(MessageFormat.format("User {0} is connected", userExternalId));
        user.persist();
        return user.id;
    }
}
