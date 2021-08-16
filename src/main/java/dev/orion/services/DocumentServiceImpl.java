package dev.orion.services;

import dev.orion.services.interfaces.DocumentService;

import javax.enterprise.context.RequestScoped;

@RequestScoped
public class DocumentServiceImpl implements DocumentService {
    @Override
    public String editContent(String content) {
        return null;
    }
}
