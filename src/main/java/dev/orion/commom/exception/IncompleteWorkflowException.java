package dev.orion.commom.exception;

public class IncompleteWorkflowException extends RuntimeException{
    public IncompleteWorkflowException(String message) {
        super(message);
    }
}
