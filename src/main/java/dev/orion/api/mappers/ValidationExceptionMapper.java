package dev.orion.api.mappers;

import dev.orion.api.endpoint.v1.dto.DefaultErrorResponseDtoV1;

import javax.validation.ConstraintViolationException;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import java.text.MessageFormat;
import java.util.logging.Logger;

@Provider
public class ValidationExceptionMapper implements ExceptionMapper<ConstraintViolationException> {
    private static final Logger LOGGER = Logger.getLogger(ValidationExceptionMapper.class.getName());


    @Override
    public Response toResponse(ConstraintViolationException exception) {
        var constraintViolations = exception.getConstraintViolations();
        DefaultErrorResponseDtoV1 defaultErrorResponseDtoV1 = new DefaultErrorResponseDtoV1();

        if (constraintViolations != null) {
            constraintViolations.forEach(constraint -> defaultErrorResponseDtoV1.addError(constraint.getMessageTemplate()));
            var errors = defaultErrorResponseDtoV1.getErrors();

            var message = MessageFormat.format("Invalid body request with the following errors: {0}", errors);
            LOGGER.warning(message);

            return Response.status(Response.Status.BAD_REQUEST)
                    .entity(defaultErrorResponseDtoV1)
                    .build();
        }

        return null;
    }

}