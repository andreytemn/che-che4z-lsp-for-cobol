package com.ca.lsp.cobol.service;

import com.broadcom.lsp.domain.cobol.event.model.RequiredCopybookEvent;
import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.DidChangeWatchedFilesParams;

import java.io.IOException;
import java.net.URISyntaxException;

public class CobolWorkspaceServiceImpl implements CobolWorkspaceService {
  @Override
  public void didChangeConfiguration(DidChangeConfigurationParams params) {
    throw new UnsupportedOperationException("Functionality not implemented yet");
  }

  @Override
  public void didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
    throw new UnsupportedOperationException("Functionality not implemented yet");
  }

  // TODO: Should be removed due to a change of responsability for this class..
  @Override
  public String getContentByCopybookName(String copybookName) throws IOException {
    return null;
  }

  // TODO: Should be removed due to a change of responsability for this class..
  @Override
  public void observerCallback(RequiredCopybookEvent adaptedDataEvent)
      throws URISyntaxException, IOException {}
}
