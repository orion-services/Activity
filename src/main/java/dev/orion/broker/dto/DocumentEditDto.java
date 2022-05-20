package dev.orion.broker.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.deser.std.UUIDDeserializer;
import com.fasterxml.jackson.databind.ser.std.UUIDSerializer;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.UUID;

public class DocumentEditDto {
    @NotNull
    public UUID uuid;

    @NotBlank
    public String externalUserId;

    @NotBlank
    public String documentContent;
}
