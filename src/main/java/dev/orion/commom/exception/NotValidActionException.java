package dev.orion.commom.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class NotValidActionException extends RuntimeException {
    private String stepName;
    private String message;

    @Override
    public String toString() {
        return "Step name: " + stepName + "has error: " + message;
    }
}
