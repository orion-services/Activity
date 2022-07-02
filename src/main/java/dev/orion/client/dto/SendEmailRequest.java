package dev.orion.client.dto;

import lombok.Getter;

import java.util.HashMap;
import java.util.Map;

@Getter
public class SendEmailRequest {
    private Map<String, String> userMessageMap = new HashMap<>();

    public void addMessage(String userExternalId, String message) {
        userMessageMap.put(userExternalId, message);
    }
}
