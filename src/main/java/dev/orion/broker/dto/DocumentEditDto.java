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
    @JsonSerialize(using = UUIDSerializer.class)
    public UUID activityId;

    @NotBlank
    public String externalUserId;

    @NotBlank
    public String documentContent;

    @NotBlank
    public  String documentId;
}
