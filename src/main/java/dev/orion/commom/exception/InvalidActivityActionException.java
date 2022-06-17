package dev.orion.commom.exception;

public class InvalidActivityActionException extends RuntimeException{
    public InvalidActivityActionException(String exceptionMessage) {
        super(exceptionMessage);
    }
}
