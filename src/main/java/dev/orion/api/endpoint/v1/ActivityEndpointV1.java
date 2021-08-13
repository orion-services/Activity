package dev.orion.api.endpoint.v1;

import com.fasterxml.jackson.annotation.JsonInclude;
import dev.orion.api.endpoint.v1.dto.CreateActivityRequestDtoV1;
import dev.orion.api.endpoint.v1.dto.CreateActivityResponseV1;
import dev.orion.data.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.UUID;

@Path("/v1/activity")
@Transactional
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
        var activity = activityService.createActivity(createActivityRequestDtoV1.getUserId());
        CreateActivityResponseV1 responseBody = new CreateActivityResponseV1();
        responseBody.setUuid(activity);

        return Response
                .status(Response.Status.CREATED)
                .entity(responseBody)
                .build();
    }
}
