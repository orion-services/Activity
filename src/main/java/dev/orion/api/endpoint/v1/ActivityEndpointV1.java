package dev.orion.api.endpoint.v1;

import dev.orion.api.endpoint.v1.dto.AddUserToActivityRequestDtoV1;
import dev.orion.api.endpoint.v1.dto.CreateActivityRequestDtoV1;
import dev.orion.api.endpoint.v1.dto.CreateActivityResponseV1;
import dev.orion.data.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.UUID;

@Path("/v1/activity")
public class ActivityEndpointV1 {

    @Inject
    ActivityService activityService;

    @GET
    @Path("/{activityUuid}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getActivity(@PathParam String activityUuid) {
        Activity activity = Activity.findById(UUID.fromString(activityUuid));
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
        activityService.addUserInActivity(UUID.fromString(activityUuid), addUserToActivityRequestDtoV1.userId);
        return Response
                .status(Response.Status.NO_CONTENT)
                .build();
    }

}
