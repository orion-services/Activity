package dev.orion.client;

import dev.orion.client.dto.CreateDocumentRequest;
import dev.orion.client.dto.CreateDocumentResponse;
import dev.orion.client.excpetion.BaseClientException;
import dev.orion.client.mapper.DocumentClientMapper;
import org.eclipse.microprofile.faulttolerance.Timeout;
import org.eclipse.microprofile.rest.client.annotation.RegisterProvider;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/api/documents")
@ApplicationScoped
@RegisterRestClient(configKey = "api.document-service.client")
@RegisterProvider(DocumentClientMapper.class)
public interface DocumentClient {

    @POST
    @Produces(MediaType.APPLICATION_JSON)
    @Timeout(1000)
    CreateDocumentResponse createDocument(CreateDocumentRequest createDocumentRequest) throws BaseClientException;
}
