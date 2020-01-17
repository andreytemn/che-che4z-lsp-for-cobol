/*
 * Copyright (c) 2019 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Broadcom, Inc. - initial API and implementation
 */
package com.ca.lsp.cobol.service;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;

@Singleton
public class MyLanguageServerImpl implements LanguageServer {
  private final TextDocumentService textService;
  private final FileSystemService fileSystemService;
  private final CobolWorkspaceService workspaceService;

  @Inject
  MyLanguageServerImpl(
      FileSystemService fileSystemService,
      TextDocumentService textService,
      CobolWorkspaceService workspaceService) {
    this.textService = textService;
    this.fileSystemService = fileSystemService;
    this.workspaceService = workspaceService;
  }

  @Override
  public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
    ServerCapabilities capabilities = new ServerCapabilities();

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full);
    capabilities.setCompletionProvider(new CompletionOptions(true, new ArrayList<>()));
    capabilities.setSignatureHelpProvider(null);
    capabilities.setDefinitionProvider(Boolean.TRUE);
    capabilities.setHoverProvider(Boolean.FALSE);
    capabilities.setCodeActionProvider(Boolean.FALSE);
    capabilities.setReferencesProvider(Boolean.TRUE);
    capabilities.setDocumentSymbolProvider(Boolean.FALSE);
    capabilities.setCodeLensProvider(null);
    capabilities.setDocumentFormattingProvider(Boolean.TRUE);
    capabilities.setDocumentHighlightProvider(Boolean.TRUE);

    WorkspaceFoldersOptions workspaceFoldersOptions = new WorkspaceFoldersOptions();
    workspaceFoldersOptions.setSupported(Boolean.TRUE);
    WorkspaceServerCapabilities workspaceServiceCapabilities =
        new WorkspaceServerCapabilities(workspaceFoldersOptions);
    capabilities.setWorkspace(workspaceServiceCapabilities);

    fileSystemService.setWorkspaceFolders(params.getWorkspaceFolders());
    return CompletableFuture.supplyAsync(() -> new InitializeResult(capabilities));
  }

  @Override
  public CompletableFuture<Object> shutdown() {
    return CompletableFuture.supplyAsync(() -> Boolean.TRUE);
  }

  @Override
  public void exit() {
    // not supported
  }

  @Override
  public TextDocumentService getTextDocumentService() {
    return textService;
  }

  @Override
  public WorkspaceService getWorkspaceService() {
    return workspaceService;
  }
}
