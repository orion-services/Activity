package dev.orion.api.endpoint;

import dev.orion.api.endpoint.dto.AddUserToActivityRequestDtoV1;
import dev.orion.api.endpoint.dto.AddUserToActivityResponseDtoV1;
import dev.orion.api.endpoint.dto.CreateActivityRequestDtoV1;
import dev.orion.api.endpoint.dto.CreateActivityResponseV1;
import dev.orion.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.commom.exceptions.UserInvalidOperationException;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponseSchema;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.UUID;

@Path("/v1/activity")
public class ActivityEndpoint {

    @Inject
    ActivityService activityService;

    @GET
    @Path("/{activityUuid}")
    @Produces(MediaType.APPLICATION_JSON)
    @APIResponseSchema(Activity.class)
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
    @APIResponseSchema(CreateActivityResponseV1.class)
    public Response createActivity(@Valid CreateActivityRequestDtoV1 createActivityRequestDtoV1) {
        var activityUuid = activityService.createActivity(createActivityRequestDtoV1.getUserExternalId(), "");
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
    @APIResponseSchema(AddUserToActivityResponseDtoV1.class)
    public Response addUserToActivity(@Valid AddUserToActivityRequestDtoV1 addUserToActivityRequestDtoV1, @PathParam String activityUuid) {
        var activity = activityService.addUserInActivity(UUID.fromString(activityUuid), addUserToActivityRequestDtoV1.userExternalId);
        var responseBody = new AddUserToActivityResponseDtoV1(activity);

        return Response
                .status(Response.Status.OK)
                .entity(responseBody)
                .build();
    }

    @PATCH
    @Path("/{activityUuid}/disconnectUser/{userExternalId}")
    public Response disconnectUserToActivity(@PathParam String activityUuid, @PathParam String userExternalId) {
        activityService.disconnectUserFromActivity(UUID.fromString(activityUuid), userExternalId);
        return Response
                .ok()
                .build();
    }


}
