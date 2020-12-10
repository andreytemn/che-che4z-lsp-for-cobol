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

import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

/** This test checks that XML query allows building the position mapping without issues. */
class TestExecSqlBuildsCorrectMapping {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. DELXML.\n"
          + "       ENVIRONMENT DIVISION.\n"
          + "       DATA DIVISION.\n"
          + "       LINKAGE SECTION.\n"
          + "       01 PARENT PIC 9.\n"
          + "       PROCEDURE DIVISION.\n"
          + "           EXEC SQL\n"
          + "            SELECT COALESCE (SUM( XMLCAST(\n"
          + "            XMLQUERY(\n"
          + "            \"declare default element namespace 'urn:oasis:names:specific\n"
          + "      -     \"ation:ubl:schema:xsd:Invoice-2';\n"
          + "\n"
          + "      -     \"declare namespace cbc='urn:oasis:names:specification:ubl:sc\n"
          + "      -     \"hema:xsd:CommonBasicComponents-2';\n"
          + "\n"
          + "      -     \"declare namespace    cac='urn:oasis:names:specification:ubl\n"
          + "      -     \":schema:xsd:CommonAggregateComponents-2';\n"
          + "\n"
          + "      -     \"/Invoice[cac:PaymentMeans/cbc:PaymentDueDate=$PAYMENTDATE]/\n"
          + "      -     \"cac:LegalMonetaryTotal/cbc:PayableAmount\"\n"
          + "\n"
          + "            PASSING DOCUMENT\n"
          + "            , :CDATE AS PAYMENTDATE)\n"
          + "            AS DECIMAL(9,2) )) ,0) INTO :SUMPAYABLE\n"
          + "            FROM UBLADMIN.UBL\n"
          + "            END-EXEC.\n"
          + "            MOVE 0 TO PARENT.\n"
          + "            GOBACK. ";

  @Test
  void test() {
    UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
