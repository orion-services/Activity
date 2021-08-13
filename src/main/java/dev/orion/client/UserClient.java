package dev.orion.client;

import dev.orion.client.dto.UserClientDto;
import org.eclipse.microprofile.faulttolerance.Timeout;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/v1/users")
@RegisterRestClient(configKey = "api.user-service.client")
public interface UserClient {

    @GET
    @Path("/{externalId}")
    @Produces(MediaType.APPLICATION_JSON)
    @Timeout(1000)
    UserClientDto getUserByExternalId(@PathParam("externalId") String externalId);
}
