package dev.orion.api.mappers;

import dev.orion.api.endpoint.v1.dto.DefaultErrorResponseDtoV1;
import dev.orion.util.exceptions.UserInvalidOperationException;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import java.util.logging.Logger;

@Provider
public class UserInvalidOperationMapper implements ExceptionMapper<UserInvalidOperationException> {
    private static final Logger LOGGER = Logger.getLogger(UserInvalidOperationException.class.getName());

    @Override
    public Response toResponse(UserInvalidOperationException exception) {
        var message = exception.getMessage();
        LOGGER.warning(message);
        DefaultErrorResponseDtoV1 errorDto = new DefaultErrorResponseDtoV1();
        errorDto.addError(message);

        return Response
                .status(Response.Status.BAD_REQUEST)
                .entity(errorDto)
                .build();
    }
}
