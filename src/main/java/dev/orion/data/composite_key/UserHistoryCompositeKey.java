package dev.orion.data.composite_key;

import javax.persistence.Embeddable;
import java.io.Serializable;
import java.util.UUID;

@Embeddable
public class UserHistoryCompositeKey implements Serializable {

    public Long activityId;
    public Long userUuid;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        UserHistoryCompositeKey that = (UserHistoryCompositeKey) o;

        if (!activityId.equals(that.activityId)) return false;
        return userUuid.equals(that.userUuid);
    }

    @Override
    public int hashCode() {
        int result = activityId.hashCode();
        result = 31 * result + userUuid.hashCode();
        return result;
    }
}
