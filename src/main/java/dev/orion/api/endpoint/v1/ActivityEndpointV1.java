package dev.orion.api.endpoint.v1;

import dev.orion.data.entity.Activity;
import dev.orion.data.entity.Document;
import dev.orion.data.entity.User;
import dev.orion.util.enums.UserStatus;
import org.jboss.resteasy.annotations.jaxrs.PathParam;

import javax.transaction.Transactional;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.UUID;

@Path("/v1/activity")
@Transactional
public class ActivityEndpointV1 {

    @GET
    @Path("/{activityUuid}")
    @Produces(MediaType.APPLICATION_JSON)
    public Activity getActivity(@PathParam String activityUuid) {
        return Activity.findById(UUID.fromString(activityUuid));
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Activity createActivity() {
        User user = new User();
        user.status = UserStatus.DISCONNECTED;

        Activity activity = new Activity();
        var document = new Document();
        document.content = "Uma atividade se inicia";

        activity.document = document;
        activity.userList.add(user);
        activity.userRound = user;

        activity.persist();
        System.out.println(activity);
        return activity;
    }
}
