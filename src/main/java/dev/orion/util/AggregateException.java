package dev.orion.util;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class AggregateException extends RuntimeException {
    List<RuntimeException> exceptions;

    public AggregateException(List<RuntimeException> exceptions) {
        this.exceptions = exceptions;
    }
}
