package dev.orion.services;

import dev.orion.client.UserClient;
import dev.orion.client.dto.FindUserListResponseDto;
import dev.orion.client.dto.FindUserListRequestDto;
import dev.orion.client.dto.UserClientDto;
import dev.orion.data.entity.User;
import dev.orion.services.dto.UserCompleteDataDto;
import dev.orion.services.dto.UserCompleteDataListDto;
import dev.orion.services.interfaces.UserService;
import org.eclipse.microprofile.rest.client.inject.RestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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

        UserClientDto userClientDto = userClient.getUserByExternalId(userExternalId);
        User userEntity = optUserEntity.orElse(null);

        if(optUserEntity.isEmpty()) {
            User user = new User(userExternalId);
            user.persist();
            userEntity = user;
        }

        return new UserCompleteDataDto(userEntity, userClientDto);
    }

    @Override
    public UserCompleteDataListDto getAllCompleteUserData(Set<String> userExternalIdList) {
        var findUserListRequestDto = new FindUserListRequestDto();
        findUserListRequestDto.uuidList.addAll(userExternalIdList);
        FindUserListResponseDto userListResponse = userClient.getAllUserByUuid(findUserListRequestDto);

        Set<UserCompleteDataDto> userCompleteDataList = new HashSet<>();
        UserCompleteDataListDto userCompleteDataListDto = new UserCompleteDataListDto();

//        Do database stuff based of found users in user client
        userListResponse.user.forEach(usr -> {
            var userExternalId = usr.uuid;
            Optional<User> optUserEntity = User.findUserByExternalId(userExternalId);
            User userEntity = optUserEntity.orElse(null);

            if (!userListResponse.usersNotFound.contains(userExternalId)) {
                if(optUserEntity.isEmpty()) {
                    User user = new User(userExternalId);
                    user.persist();
                    userEntity = user;
                }

                userCompleteDataList.add(new UserCompleteDataDto(userEntity, usr));
            }
        });

        userCompleteDataListDto.userList.addAll(userCompleteDataList);
        userCompleteDataListDto.usersNotFound.addAll(userListResponse.usersNotFound);
        return userCompleteDataListDto;
    }
}
