       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECALC.
      * This program reads a file of input values into INVALS-WS
      * The operation read into the W-S structure drives the arithmetic
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVALS
           ASSIGN TO UT-S-INVALS
             ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INVALS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INVALS-REC.
       01  INVALS-REC  PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01  INVALS-EOF               PIC X(01) VALUE SPACE.
           88  END-OF-FILE          VALUE "Y".
       01  INVALS-WS.
           05  OPERATION            PIC X(01).
               88  OP-ADD           VALUE "A".
               88  OP-SUBTRACT      VALUE "S".
               88  OP-MULTIPLY      VALUE "M".
               88  OP-SQUARE-ROOT   VALUE "Q".
               88  OP-DIVIDE        VALUE "D".
           05  INVALS-1             PIC S99V99.
           05  INVALS-2             PIC S99.
           05  INVALS-RESULT        PIC S99999V99.

       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL END-OF-FILE.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
           INITIALIZE INVALS-WS.
           PERFORM 300-OPEN-FILES.
      * Priming Read
           PERFORM 400-Read-INVALS.
       100-Main.
           DISPLAY ">>>>><<<<<".
           DISPLAY "Operation: " OPERATION.
           DISPLAY "Invals-1: " INVALS-1.
           DISPLAY "Invals-2: " INVALS-2.
           IF OP-ADD PERFORM 500-ADD
           ELSE IF OP-SUBTRACT PERFORM 600-SUBTRACT
           ELSE IF OP-MULTIPLY PERFORM 700-MULTIPLY
           ELSE IF OP-SQUARE-ROOT PERFORM 750-SQUARE-ROOT
           ELSE IF OP-DIVIDE PERFORM 800-DIVIDE.
           DISPLAY "Invals Result: " INVALS-RESULT.
           PERFORM 400-Read-INVALS.
       300-Open-Files.
           OPEN INPUT INVALS.
       400-Read-INVALS.
           READ INVALS INTO INVALS-WS
      * Set AT END Switch
               AT END MOVE "Y" TO INVALS-EOF
           END-READ.
       500-ADD.
           ADD INVALS-1, INVALS-2 GIVING INVALS-RESULT.
       600-SUBTRACT.
           SUBTRACT INVALS-2 FROM INVALS-1 GIVING INVALS-RESULT.
       700-MULTIPLY.
           MULTIPLY INVALS-1 BY INVALS-2 GIVING INVALS-RESULT.
       750-SQUARE-ROOT.
           IF INVALS-1 IS POSITIVE THEN
               COMPUTE INVALS-RESULT = (INVALS-1 ** .5).
           IF INVALS-1 IS NEGATIVE THEN
               MOVE 0 TO INVALS-RESULT
               DISPLAY "Unable to square root negative number.".
       800-DIVIDE.
           DIVIDE INVALS-2 BY INVALS-1 GIVING INVALS-RESULT.
       900-CLOSE-FILES.
           CLOSE INVALS.