package dev.orion.broker.dto;

import lombok.Getter;

import java.util.HashMap;
import java.util.Map;

@Getter
public class SendEmailProduceBody {
    private Map<String, String> userMessageMap = new HashMap<>();

    public void addMessage(String userExternalId, String message) {
        userMessageMap.put(userExternalId, message);
    }
}
