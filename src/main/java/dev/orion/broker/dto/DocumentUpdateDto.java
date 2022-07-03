package dev.orion.broker.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.UUID;

@Data
@AllArgsConstructor
public class DocumentUpdateDto {
    public UUID id;
    public String  content;
    public String externalUserId;
}
