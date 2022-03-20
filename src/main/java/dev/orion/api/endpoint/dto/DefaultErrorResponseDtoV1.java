package dev.orion.api.endpoint.dto;

import com.fasterxml.jackson.annotation.JsonFormat;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class DefaultErrorResponseDtoV1 {
    private List<String> errors = new ArrayList<>();

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy hh:mm:ss")
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
