package dev.orion.fixture;

import dev.orion.client.dto.UserClientDto;
import dev.orion.commom.enums.UserStatus;
import dev.orion.entity.User;
import dev.orion.services.dto.UserEnhancedWithExternalDataDto;
import net.datafaker.Faker;

import java.util.UUID;

final public class UserFixture {
    static public User generateUser() {
        User user = new User();
        user.id = Faker.instance().number().randomNumber();
        user.externalId = UUID.randomUUID().toString();
        user.status = UserStatus.AVAILABLE;

        return user;
    }

    static public UserClientDto generateClientDto() {
        UserClientDto userClientDto = new UserClientDto();
        userClientDto.isActive = true;
        userClientDto.name = Faker.instance().funnyName().name();

        return  userClientDto;
    }

    static public UserEnhancedWithExternalDataDto generateUserEnhancedWithExternalDataDto() {
        var user = generateUser();
        var userClientDto = generateClientDto();

        return new UserEnhancedWithExternalDataDto(user, userClientDto);
    }
}
