/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */

package com.broadcom.lsp.cobol.usecases;

import com.broadcom.lsp.cobol.service.delegates.validations.SourceInfoLevels;
import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

/**
 * This test checks that parser can find and underline an incorrect variable structure. Here CHILD1
 * is not a part of PARENT because they are on the same level, but on the line 8 they called the way
 * if CHILD1 was a part of PARENT1 structure. It is incorrect, so CHILD1 should be underlined. On
 * the line 10 there is a non-defined variable called as a part of a structure, that should produce
 * two errors with intersecting positions. Check also that the multiple whitespaces collapsed in the
 * error message.
 */
// TODO:fix doc
class TestEntireIncorrectVariableStructureUnderlined {

  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. TESTREPL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       01  {$*PARENT} PIC 9.\n"
          + "       01  {$*CHILD1} PIC 9.\n"
          + "       PROCEDURE DIVISION.\n"
          + "       {#*MAINLINE}.\n"
          + "           MOVE 0 TO   {CHILD1|inv} OF PARENT.\n"
          + "           MOVE 0 TO   {CHILD2|single} OF PARENT.\n"
          + "           GOBACK. ";

  @Test
  void test() {
    UseCaseEngine.runTest(
        TEXT,
        List.of(),
        Map.of(
            "inv",
                new Diagnostic(
                    null, "Invalid definition for: CHILD1", DiagnosticSeverity.Error, SourceInfoLevels.ERROR.getText()),
            "single",
                new Diagnostic(
                    null, "Invalid definition for: CHILD2", DiagnosticSeverity.Error, SourceInfoLevels.ERROR.getText())));
  }
}
