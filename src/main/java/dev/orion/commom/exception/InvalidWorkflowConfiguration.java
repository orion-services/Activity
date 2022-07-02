package dev.orion.commom.exception;

public class InvalidWorkflowConfiguration extends RuntimeException{
    public InvalidWorkflowConfiguration(String message) {
        super(message);
    }
}
