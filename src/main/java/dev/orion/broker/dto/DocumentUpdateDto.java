package dev.orion.broker.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.deser.std.UUIDDeserializer;
import lombok.AllArgsConstructor;
import lombok.Data;

import javax.validation.constraints.NotNull;
import java.util.UUID;

@Data
@AllArgsConstructor
public class DocumentUpdateDto {
    @NotNull
    public String id;
    @NotNull
    public String content;
    @NotNull
    public String externalUserId;
    @NotNull
    @JsonDeserialize(using = UUIDDeserializer.class)
    public UUID messageKey;
}
