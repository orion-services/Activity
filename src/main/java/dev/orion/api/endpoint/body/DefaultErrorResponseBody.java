package dev.orion.api.endpoint.body;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class DefaultErrorResponseBody {
    private List<String> errors = new ArrayList<>();

    private final LocalDateTime occurredAt = LocalDateTime.now();

    public void addError(String error) {
        this.errors.add(error);
    }

    public List<String> getErrors() {
        return errors;
    }

    public LocalDateTime getOccurredAt() {
        return occurredAt;
    }

    public void setErrors(List<String> errors) {
        this.errors = errors;
    }
}
