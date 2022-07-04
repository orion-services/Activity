package dev.orion.client.excpetion;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.net.URI;

@AllArgsConstructor
@Getter
@Setter
public class BaseClientException extends RuntimeException{
    Integer code;
    URI location;
    String errorMessage;
}
