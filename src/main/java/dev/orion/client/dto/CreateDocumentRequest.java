package dev.orion.client.dto;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.UUIDSerializer;
import lombok.AllArgsConstructor;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import java.util.UUID;

@Data
@AllArgsConstructor
public class CreateDocumentRequest {
    @JsonSerialize(using = UUIDSerializer.class)
    @NotBlank
    private UUID id;
    private String initialText;
}
