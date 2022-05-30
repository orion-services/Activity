package dev.orion.fixture;

import dev.orion.client.dto.UserClientResponse;
import dev.orion.commom.enums.UserStatus;
import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalData;
import net.datafaker.Faker;

import java.util.Arrays;
import java.util.UUID;

final public class UserFixture {
    static public User generateUser() {
        User user = new User();
        user.externalId = UUID.randomUUID().toString();
        user.status = UserStatus.AVAILABLE;

        return user;
    }

    static public UserClientResponse generateClienResponsetDto() {
        UserClientResponse userClientResponse = new UserClientResponse();
        userClientResponse.isActive = true;
        userClientResponse.name = Faker.instance().funnyName().name();
        userClientResponse.role = Arrays.asList(new String[]{""});

        return userClientResponse;
    }

    static public UserEnhancedWithExternalData generateUserEnhancedWithExternalDataDto() {
        var user = generateUser();
        var userClientDto = generateClienResponsetDto();

        return new UserEnhancedWithExternalData(user, userClientDto);
    }
}
