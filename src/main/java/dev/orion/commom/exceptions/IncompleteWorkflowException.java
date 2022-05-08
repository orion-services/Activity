package dev.orion.commom.exceptions;

public class IncompleteWorkflowException extends RuntimeException{
    public IncompleteWorkflowException(String message) {
        super(message);
    }
}
