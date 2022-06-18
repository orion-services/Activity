package dev.orion.api.endpoint;

import dev.orion.api.endpoint.body.AddUserToActivityRequestBody;
import dev.orion.api.endpoint.body.AddUserToActivityResponseBody;
import dev.orion.api.endpoint.body.CreateActivityRequestBody;
import dev.orion.api.endpoint.body.CreateActivityResponseBody;
import dev.orion.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import dev.orion.commom.exception.UserInvalidOperationException;
import lombok.val;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponseSchema;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.UUID;
import java.util.stream.Collectors;

@Path("/v1/activities")
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
    @APIResponseSchema(CreateActivityResponseBody.class)
    public Response createActivity(@Valid CreateActivityRequestBody createActivityRequestBody) {
        val activityUuid = activityService.createActivity(createActivityRequestBody.getUserExternalId(), createActivityRequestBody.getWorkflowName());
        val activity = (Activity) Activity.findById(activityUuid);

        val responseBody = new CreateActivityResponseBody();
        responseBody.setUuid(activityUuid);
        responseBody.setGroups(activity.getGroupActivities().stream().map(groupActivity -> groupActivity.getUuid()).collect(Collectors.toList()));

        return Response
                .status(Response.Status.CREATED)
                .entity(responseBody)
                .build();
    }

    @POST
    @Path("/{activityUuid}/addUser")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @APIResponseSchema(AddUserToActivityResponseBody.class)
    public Response addUserToActivity(@Valid AddUserToActivityRequestBody addUserToActivityRequestBody, @PathParam String activityUuid) {
        var activity = activityService.addUserInActivity(UUID.fromString(activityUuid), addUserToActivityRequestBody.userExternalId);
        var responseBody = new AddUserToActivityResponseBody(activity);

        return Response
                .status(Response.Status.OK)
                .entity(responseBody)
                .build();
    }

    @PATCH
    @Path("/{activityUuid}/start")
    public Response startActivity(@PathParam String activityUuid) {
        return Response.status(Response.Status.SERVICE_UNAVAILABLE).build();
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
