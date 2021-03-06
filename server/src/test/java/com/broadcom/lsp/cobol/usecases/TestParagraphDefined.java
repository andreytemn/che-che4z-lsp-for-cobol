package com.broadcom.lsp.cobol.usecases;

import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

/** This test checks that performing defined paragraph processed correctly */
class TestParagraphDefined {
  private static final String TEXT =
"       IDENTIFICATION DIVISION.\n" +
        "          PROGRAM-ID. TEST1.\n" +
        "          DATA DIVISION.\n" +
        "          WORKING-STORAGE SECTION.\n" +
        "          PROCEDURE DIVISION.\n" +
        "            PERFORM {#GET-DATA}.\n" +
        "            STOP RUN.\n" +
        "          {#*GET-DATA}.\n" +
        "            DISPLAY \"\".";
    @Test
    void test() {
        UseCaseEngine.runTest(TEXT, List.of(), Map.of());
    }

}
