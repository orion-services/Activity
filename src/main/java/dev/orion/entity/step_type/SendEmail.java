package dev.orion.entity.step_type;

import dev.orion.commom.constant.ActivityStage;
import dev.orion.entity.Step;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.CollectionTable;
import javax.persistence.DiscriminatorValue;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Entity
@Getter
@Setter
@DiscriminatorValue("SEND_EMAIL")
public class SendEmail extends Step {
    @Getter(AccessLevel.NONE)
    final private String type = "SEND_EMAIL";
    final private String description = "Send email to advice when activity start or ends";

    @ElementCollection
    @CollectionTable(name = "ACTIVITY_STAGE_EMAIL_MESSAGE")
    private Map<ActivityStage, String> activityStageMessageMap = new HashMap<>();

    public SendEmail() {
        allowedStages = Set.of(ActivityStage.PRE, ActivityStage.POS);
    }

    public void addMessage(ActivityStage activityStage, String message) {
        activityStageMessageMap.put(activityStage, message);
    }
}
