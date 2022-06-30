package dev.orion.commom.constant;

public enum UserStatus {
    CONNECTED("CONNECTED"), AVAILABLE("AVAILABLE"), DISCONNECTED("DISCONNECTED");

    public final String status;

    private UserStatus(String status) {
        this.status = status;
    }
}
