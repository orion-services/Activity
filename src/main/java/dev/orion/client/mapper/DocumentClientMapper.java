package dev.orion.client.mapper;

import dev.orion.client.excpetion.BaseClientException;
import lombok.val;
import org.eclipse.microprofile.rest.client.ext.ResponseExceptionMapper;

import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;
import java.text.MessageFormat;
import java.util.Optional;
import java.util.logging.Logger;

@Provider
public class DocumentClientMapper implements ResponseExceptionMapper<BaseClientException> {
    private static final Logger LOGGER = Logger.getLogger(DocumentClientMapper.class.getName());

    @Override
    public BaseClientException toThrowable(Response response) {
        val status = response.getStatus();
        val message = MessageFormat.format("The request to {0} has fail with code: {1}", response.getLocation(), status);
        LOGGER.warning(message);
        val entity = Optional.ofNullable(response.getEntity()).orElse("");
        return new BaseClientException(status, response.getLocation(), entity.toString());
    }

    @Override
    public boolean handles(int status, MultivaluedMap<String, Object> headers) {
        return status >= 400 && status < 500;
    }

    @Override
    public int getPriority() {
        return ResponseExceptionMapper.super.getPriority();
    }
}
