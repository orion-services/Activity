package dev.orion.api.mappers;

import dev.orion.api.endpoint.dto.DefaultErrorResponseDtoV1;
import dev.orion.commom.exception.UserInvalidOperationException;

import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import java.util.logging.Logger;

@Provider
public class NotFoundInvalidOperationMapper implements ExceptionMapper<NotFoundException> {
    private static final Logger LOGGER = Logger.getLogger(NotFoundInvalidOperationMapper.class.getName());

    @Override
    public Response toResponse(NotFoundException exception) {
        final var message = exception.getMessage();
        LOGGER.warning(message);
        DefaultErrorResponseDtoV1 errorDto = new DefaultErrorResponseDtoV1();
        errorDto.addError(message);

        return Response
                .status(Response.Status.BAD_REQUEST)
                .entity(errorDto)
                .build();
    }
}
