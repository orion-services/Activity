package dev.orion.fixture;

import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.enums.UserStatus;
import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalDataResponse;
import net.datafaker.Faker;

import java.util.UUID;

final public class UserFixture {
    static public User generateUser() {
        User user = new User();
        user.externalId = UUID.randomUUID().toString();
        user.status = UserStatus.AVAILABLE;

        return user;
    }

    static public UserClientResponse generateClientDto() {
        UserClientResponse userClientResponse = new UserClientResponse();
        userClientResponse.isActive = true;
        userClientResponse.name = Faker.instance().funnyName().name();

        return userClientResponse;
    }

    static public UserEnhancedWithExternalDataResponse generateUserEnhancedWithExternalDataDto() {
        var user = generateUser();
        var userClientDto = generateClientDto();

        return new UserEnhancedWithExternalDataResponse(user, userClientDto);
    }
}
