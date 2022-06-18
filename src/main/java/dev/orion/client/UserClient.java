package dev.orion.client;

import dev.orion.client.dto.UserClientResponse;
import org.eclipse.microprofile.faulttolerance.Timeout;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/api/users")
@ApplicationScoped
@RegisterRestClient(configKey = "api.user-service.client")
public interface UserClient {
    @GET
    @Path("/{externalId}")
    @Produces(MediaType.APPLICATION_JSON)
    @Timeout(1000)
    UserClientResponse getUserByExternalId(@PathParam("externalId") String externalId);

    @GET
    @Path("/")
    @Produces(MediaType.APPLICATION_JSON)
    @Timeout(1000)
    List<UserClientResponse> getAllUsers();
}
