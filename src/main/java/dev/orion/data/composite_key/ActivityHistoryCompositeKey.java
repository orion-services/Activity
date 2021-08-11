package dev.orion.data.composite_key;

import javax.persistence.Embeddable;
import java.io.Serializable;
import java.util.UUID;

@Embeddable
public class ActivityHistoryCompositeKey implements Serializable {
    public Long activityId;
    public UUID activityUUId;
}
