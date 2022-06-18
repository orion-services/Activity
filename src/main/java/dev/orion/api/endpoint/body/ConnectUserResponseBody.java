package dev.orion.api.endpoint.body;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ConnectUserResponseBody {
    String userExternalId;
}
