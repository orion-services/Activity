package dev.orion.api.endpoint;

import dev.orion.api.endpoint.body.*;
import dev.orion.commom.exception.UserInvalidOperationException;
import dev.orion.entity.Activity;
import dev.orion.services.interfaces.ActivityService;
import lombok.val;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponseSchema;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.jboss.resteasy.annotations.jaxrs.PathParam;
import org.jboss.resteasy.reactive.ResponseStatus;

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
    @ResponseStatus(200)
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
    @APIResponses({
            @APIResponse(
                    content = {@Content(schema = @Schema(implementation = CreateActivityResponseBody.class))},
                    description = "Create a new activity",
                    responseCode = "201"
            ),
            @APIResponse(
                    content = {@Content(schema = @Schema(implementation = DefaultErrorResponseBody.class))},
                    description = "List of errors of trying add a invalid user",
                    responseCode = "400"
            )
    })
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
    @APIResponses({
            @APIResponse(
                    content = {@Content(schema = @Schema(implementation = AddUserToActivityResponseBody.class))},
                    description = "Add user into activity to participate.",
                    responseCode = "200"
            ),
            @APIResponse(
                    content = {@Content(schema = @Schema(implementation = DefaultErrorResponseBody.class))},
                    description = "List of errors of trying add a invalid user",
                    responseCode = "400"
            )
    })

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
    @APIResponse(
            responseCode = "200",
            description = "Start a activity",
            content = @Content(schema = @Schema(implementation = StartActivityResponseBody.class))
    )
    @Produces(MediaType.APPLICATION_JSON)
    public Response startActivity(@PathParam String activityUuid) {
        val activity = activityService.startActivity(UUID.fromString(activityUuid));
        return Response
                .ok(new StartActivityResponseBody(activity))
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
