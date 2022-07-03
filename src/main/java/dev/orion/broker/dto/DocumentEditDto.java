package dev.orion.broker.dto;

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

    @NotBlank
    public  String documentUUID;
}
