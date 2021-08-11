package dev.orion.data.composite_key;

import javax.persistence.Embeddable;
import java.io.Serializable;
import java.util.UUID;

@Embeddable
public class UserHistoryCompositeKey implements Serializable {

    public Long activityId;
    public UUID userUuid;
}
