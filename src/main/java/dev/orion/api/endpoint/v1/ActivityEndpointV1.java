package dev.orion.api.endpoint.v1;

import dev.orion.data.entity.Activity;

import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/v1/activity")
@Transactional
public class ActivityEndpointV1 {

    @Inject
    Activity activity;

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String getActivity() {

        return "Just an activity";
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public void createActivity() {

    }
}
