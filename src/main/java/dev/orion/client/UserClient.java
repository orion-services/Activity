package dev.orion.client;

import dev.orion.client.dto.FindUserListResponseDto;
import dev.orion.client.dto.FindUserListRequestDto;
import dev.orion.client.dto.UserClientDto;
import org.eclipse.microprofile.faulttolerance.Timeout;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/v1/service")
@ApplicationScoped
@RegisterRestClient(configKey = "api.user-service.client")
public interface UserClient {

    @GET
    @Path("/{externalId}")
    @Produces(MediaType.APPLICATION_JSON)
    @Timeout(1000)
    UserClientDto getUserByExternalId(@PathParam("externalId") String externalId);

    @POST
    @Path("/find-by-uuid")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    FindUserListResponseDto getAllUserByUuid(FindUserListRequestDto findUserListRequestDto);


}
