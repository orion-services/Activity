package dev.orion.commom.exception;


import lombok.val;

import java.util.LinkedList;
import java.util.List;

public class UserInvalidOperationException extends RuntimeException {
    public UserInvalidOperationException(String message) {
        super(message);
    }

    public UserInvalidOperationException(String messagePrefix, List<String> messages) {
        super(compactMessagesIntoSingle(messagePrefix,messages));
    }

    public static String compactMessagesIntoSingle(String messagePrefix, List<String> messages) {
        val linkedMessages = new LinkedList<>(messages);
        val firstMessage = linkedMessages.poll();
        String exceptionMessage = messagePrefix + firstMessage;

        val stringBuilder = new StringBuilder(exceptionMessage);
        linkedMessages.forEach((msg) -> {
            if (stringBuilder.length() == 0) {
                stringBuilder.append(msg);
            } else if(linkedMessages.indexOf(msg) == linkedMessages.size() - 1) {
                stringBuilder.append(" and ").append(msg);
            } else {
                stringBuilder.append(", ").append(msg);
            }
        });

        return stringBuilder.toString();
    }
}
