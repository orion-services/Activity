package dev.orion.api.endpoint.v1;

import dev.orion.api.endpoint.v1.dto.AddUserToActivityRequestDtoV1;
import dev.orion.api.endpoint.v1.dto.AddUserToActivityResponseDtoV1;
import dev.orion.api.endpoint.v1.dto.CreateActivityRequestDtoV1;
import dev.orion.api.endpoint.v1.dto.CreateActivityResponseV1;
import dev.orion.data.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.util.exceptions.UserInvalidOperationException;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.UUID;

@Path("/v1/activity")
public class ActivityEndpointV1 {

    @Inject
    ActivityService activityService;

    @GET
    @Path("/{activityUuid}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response findActivity(@PathParam String activityUuid) {
        Activity activity = (Activity) Activity
                .findByIdOptional(UUID.fromString(activityUuid))
                .orElseThrow(() -> new UserInvalidOperationException(
                        MessageFormat.format("Activity {0} not found", activityUuid)));
        return Response
                .ok(activity)
                .build();
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Response createActivity(@Valid CreateActivityRequestDtoV1 createActivityRequestDtoV1) {
        var activityUuid = activityService.createActivity(createActivityRequestDtoV1.getUserId());
        CreateActivityResponseV1 responseBody = new CreateActivityResponseV1();
        responseBody.setUuid(activityUuid);

        return Response
                .status(Response.Status.CREATED)
                .entity(responseBody)
                .build();
    }

    @POST
    @Path("/{activityUuid}/addUser")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Response addUserToActivity(AddUserToActivityRequestDtoV1 addUserToActivityRequestDtoV1, @PathParam String activityUuid) {
        var activity = activityService.addUserInActivity(UUID.fromString(activityUuid), addUserToActivityRequestDtoV1.userId);
        var responseBody = new AddUserToActivityResponseDtoV1(activity);

        return Response
                .status(Response.Status.OK)
                .entity(responseBody)
                .build();
    }

    @POST
    @Path("/{activityUuid}/disconnectUser/{userUuid}")
    public Response disconnectUserToActivity(@PathParam String activityUuid, @PathParam String userUuid) {

        return Response
                .ok()
                .build();
    }


}
