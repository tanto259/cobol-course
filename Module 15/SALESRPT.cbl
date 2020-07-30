       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SALESRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO UT-S-SALESFL
                  ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01  SALES-TABLE.
           05 REGION.
               10 Q1-SALES          PIC 9(5)V99.
               10 Q2-SALES          PIC 9(5)V99.
               10 Q3-SALES          PIC 9(5)V99.
               10 Q4-SALES          PIC 9(5)V99.
           05 FILLER                PIC X(52).
       WORKING-STORAGE SECTION.
       01  SWITCHES-IN-PROGRAM.
           05  SW-END-OF-DATA          PIC X VALUE 'N'.
               88  END-OF-DATA               VALUE 'Y'.
       01  WS-TEMP-COMPUTE.
           05  WS-TEMP-VAL          PIC 9(7)V99.
           05  WS-TEMP-OUTPUT       PIC $$,$$$,$$9.99.
       01  WS-SALES-TABLE-O.
           05  WS-Q1-SALES-O        PIC $$$,$$9.99.
           05  WS-Q2-SALES-O        PIC $$$,$$9.99.
           05  WS-Q3-SALES-O        PIC $$$,$$9.99.
           05  WS-Q4-SALES-O        PIC $$$,$$9.99.
       01  WS-SALES-TABLE.
          05 WS-REGION OCCURS 4 TIMES.
               10  WS-Q1-SALES          PIC 9(5)V99 VALUE ZEROES.
               10  WS-Q2-SALES          PIC 9(5)V99 VALUE ZEROES.
               10  WS-Q3-SALES          PIC 9(5)V99 VALUE ZEROES.
               10  WS-Q4-SALES          PIC 9(5)V99 VALUE ZEROES.
       77  IDX                      PIC 9(4) COMP.
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-RECORDS.
           PERFORM 300-WRAP-UP.
           GOBACK.
       100-INITIALIZATION.
           OPEN INPUT  SALES-FILE.
           INITIALIZE WS-SALES-TABLE.
           READ SALES-FILE
           AT END MOVE 'Y' TO SW-END-OF-DATA.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
                MOVE Q1-SALES TO
                        WS-Q1-SALES (IDX)
                MOVE Q2-SALES TO
                        WS-Q2-SALES (IDX)
                MOVE Q3-SALES TO
                        WS-Q3-SALES (IDX)
                MOVE Q4-SALES TO
                        WS-Q4-SALES (IDX)
                READ SALES-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-DATA
                END-READ
           END-PERFORM.
       200-PROCESS-RECORDS.
           DISPLAY "SALES REPORT".
           DISPLAY "------------".
           COMPUTE WS-TEMP-VAL = FUNCTION SUM(WS-Q1-SALES(ALL)).
           MOVE WS-TEMP-VAL TO WS-TEMP-OUTPUT.
           DISPLAY "TOTAL Q1 SALES OF ALL REGION : " WS-TEMP-OUTPUT.
           COMPUTE WS-TEMP-VAL = FUNCTION MAX(WS-Q1-SALES(ALL)).
           MOVE WS-TEMP-VAL TO WS-TEMP-OUTPUT.
           DISPLAY "MAX Q1 SALES OF ALL REGION   : " WS-TEMP-OUTPUT.
           COMPUTE WS-TEMP-VAL = FUNCTION MIN(WS-Q1-SALES(ALL)).
           MOVE WS-TEMP-VAL TO WS-TEMP-OUTPUT.
           DISPLAY "MIN Q1 SALES OF ALL REGION   : " WS-TEMP-OUTPUT.
           DISPLAY " ".
           DISPLAY "SALES BREAKOUT".
           DISPLAY "--------------".
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
               DISPLAY " "
               DISPLAY "REGION - " IDX
               DISPLAY "------"
               MOVE WS-Q1-SALES (IDX) TO WS-Q1-SALES-O
               MOVE WS-Q2-SALES (IDX) TO WS-Q2-SALES-O
               MOVE WS-Q3-SALES (IDX) TO WS-Q3-SALES-O
               MOVE WS-Q4-SALES (IDX) TO WS-Q4-SALES-O
               DISPLAY "Q1 SALES    :  " WS-Q1-SALES-O
               DISPLAY "Q2 SALES    :  " WS-Q2-SALES-O
               DISPLAY "Q3 SALES    :  " WS-Q3-SALES-O
               DISPLAY "Q4 SALES    :  " WS-Q4-SALES-O
               COMPUTE WS-TEMP-VAL = WS-Q1-SALES (IDX)
                   + WS-Q2-SALES (IDX) + WS-Q3-SALES (IDX)
                   + WS-Q4-SALES (IDX)
               MOVE WS-TEMP-VAL TO WS-TEMP-OUTPUT
               DISPLAY "------"
               DISPLAY "TOTAL SALES :  " WS-TEMP-OUTPUT
           END-PERFORM.
           DISPLAY " ".
           DISPLAY "--------------"
           DISPLAY " ".
           COMPUTE WS-TEMP-VAL = FUNCTION SUM(WS-Q1-SALES(ALL)) +
               FUNCTION SUM(WS-Q2-SALES(ALL)) +
               FUNCTION SUM(WS-Q3-SALES(ALL)) +
               FUNCTION SUM(WS-Q4-SALES(ALL)).
           MOVE WS-TEMP-VAL TO WS-TEMP-OUTPUT.
           DISPLAY "GRAND TOTAL SALES : " WS-TEMP-OUTPUT.
       300-WRAP-UP.
           CLOSE SALES-FILE.
