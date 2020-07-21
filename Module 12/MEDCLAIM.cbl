       IDENTIFICATION DIVISION.
       PROGRAM-ID.     MEDCLAIM.
       AUTHOR.         HARTANTO.

      ******************************************************************
      ***** THIS PROGRAM WRITE RECORDS FOR ALL MEDICAL CLAIM THAT ARE
      ***** IN-POLICY.
      *****
      ***** IF THE PROGRAM ENCOUNTERS AN INVALID RECORD, THE RECORD
      ***** WILL NOT BE PRINTED INTO THE RECORD.
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMFILE
           ASSIGN TO UT-S-CLAIM
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS CLAIMFILE-ST.
           SELECT PRINTFILE
           ASSIGN TO CLAIMRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PRINTFILE-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMFILE
           RECORD CONTAINS 90 CHARACTERS.
       01  CLAIMFILE-REC               PIC X(90).

       FD  PRINTFILE
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINTFILE-REC               PIC X(133).

       WORKING-STORAGE SECTION.
           COPY CLAIMREC.

       01  FLAGS.
           05  CLAIMFILE-EOF           PIC X(01)       VALUE 'N'.
               88  END-REACHED                         VALUE 'Y'.
           05  CLAIMFILE-ST            PIC X(02).
               88  CF-NORMAL                           VALUE '00'.
           05  PRINTFILE-ST            PIC X(02).
               88  PF-NORMAL                           VALUE '00'.
           05  DEDUCTIBLE-ST           PIC X(01)       VALUE 'N'.
               88  DEDUCTIBLE-MET                      VALUE 'Y'.
           05  RECORD-ST               PIC X(01)       VALUE 'Y'.
               88  VALID-RECORD                        VALUE 'Y'.
               88  INVALID-RECORD                      VALUE 'N'.

       01  HEADER-LINE-01.
           05  HDR-CURRENT-DATE        PIC XXXX/XX/XX.
           05  FILLER                  PIC X(45)       VALUE SPACES.
           05  FILLER                  PIC X(25)
                    VALUE 'Group Claims Daily Totals'.

       01  HEADER-LINE-02.
           05  FILLER                  PIC X(20)       VALUE 'POLICY'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE 'POLICY'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE 'FIRST'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE 'LAST'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE 'RENEW'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(05)       VALUE 'DEDUC'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE 'COPAY'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(06)       VALUE 'DEDUC'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(13)       VALUE 'CLAIM'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE 'CLAIM'.

       01  HEADER-LINE-03.
           05  FILLER                  PIC X(20)       VALUE 'TYPE'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE 'NO'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE 'NAME'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE 'NAME'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE 'DATE'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(05)       VALUE 'MET'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE 'PERCENT'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(06)       VALUE 'AMOUNT'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(13)       VALUE 'AMOUNT'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE 'PAID'.

       01  HEADER-LINE-04.
           05  FILLER                  PIC X(20)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(05)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(06)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(13)       VALUE ALL '-'.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE ALL '-'.

       01  CONTENT-LINE.
           05  CON-POLICY-TYPE         PIC X(20).
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  CON-POLICY-NO           PIC 99B999B99.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  CON-NAME.
               10  CON-FNAME           PIC X(10).
               10  FILLER              PIC X(03)       VALUE SPACES.
               10  CON-LNAME           PIC X(15).
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  CON-RENEW-DATE          PIC XXXX/XX/XX.
           05  FILLER                  PIC X(05)       VALUE SPACES.
           05  CON-DEDUC-MET           PIC X(01).
           05  FILLER                  PIC X(07)       VALUE SPACES.
           05  CON-COPAY-PCTG          PIC .9(03).
           05  FILLER                  PIC X(05)       VALUE SPACES.
           05  CON-DEDUC-AMNT          PIC $$$9.
           05  FILLER                  PIC X(05)       VALUE SPACES.
           05  CON-CLAIM-AMNT          PIC $$,$$$,$$9.99.
           05  FILLER                  PIC X(03)       VALUE SPACES.
           05  CON-CLAIM-PAID          PIC $$,$$9.99.
           05  FILLER                  PIC X(03)       VALUE SPACES.

       01  TEMP-CALC-STORAGE.
           05  DEDUCTIBLE-WS           PIC 9(04).
           05  CLAIMPAID-WS            PIC 9(05)V9(02).
           05  DEDUC-PCTG              PIC V9(03)      VALUE .002.

       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-PROCESS-CLAIM UNTIL END-REACHED.
           PERFORM 900-CLEANUP.
           GOBACK.

       100-HOUSEKEEPING.
           MOVE FUNCTION CURRENT-DATE TO HDR-CURRENT-DATE.
           PERFORM 300-OPEN-FILES.
           PERFORM 550-PRINT-HEADERS.
           PERFORM 400-READ-CLAIM.

       200-PROCESS-CLAIM.
           IF VALID-RECORD
                PERFORM 250-CALCULATE-CLAIM
           END-IF.

           PERFORM 400-READ-CLAIM.

       250-CALCULATE-CLAIM.
           PERFORM 250-CALCULATE-DEDUCTIBLE.
           IF DEDUCTIBLE-MET
                COMPUTE CLAIMPAID-WS ROUNDED =
                    CLAIM-AMOUNT - (POLICY-COINSURANCE * CLAIM-AMOUNT)
           ELSE
                COMPUTE CLAIMPAID-WS ROUNDED = CLAIM-AMOUNT -
                    DEDUCTIBLE-WS - (POLICY-COINSURANCE * CLAIM-AMOUNT)
           END-IF.

           SUBTRACT CLAIMPAID-WS FROM POLICY-AMOUNT.

           IF POLICY-AMOUNT > ZERO
                PERFORM 500-PRINT-RECORD
           END-IF.

       250-CALCULATE-DEDUCTIBLE.
           MOVE 'N' TO DEDUCTIBLE-ST.

           COMPUTE DEDUCTIBLE-WS ROUNDED =
                POLICY-AMOUNT * DEDUC-PCTG.

           IF POLICY-DEDUCTIBLE-PAID >= DEDUCTIBLE-WS
                MOVE 'Y' TO DEDUCTIBLE-ST
           END-IF.


       300-OPEN-FILES.
           OPEN INPUT CLAIMFILE
           IF NOT CF-NORMAL
                DISPLAY 'ERROR OPENING CLAIMFILE'
                GO TO 999-RETURN-ERR.

           OPEN OUTPUT PRINTFILE
           IF NOT PF-NORMAL
                DISPLAY 'ERROR OPENING PRINTFILE'
                GO TO 999-RETURN-ERR.

       400-READ-CLAIM.
           MOVE 'Y' TO RECORD-ST.

           READ CLAIMFILE INTO CLAIM-REC
           AT END
                MOVE 'Y' TO CLAIMFILE-EOF
           END-READ.

           IF CF-NORMAL OR END-REACHED
                NEXT SENTENCE
           ELSE
                DISPLAY 'ERROR READING CLAIMFILE'
                GO TO 999-RETURN-ERR
           END-IF.

           PERFORM 450-CHECK-RECORD.

       450-CHECK-RECORD.
           IF NOT VALID-POLICY
                MOVE 'N' TO RECORD-ST.
           IF INS-POLICY-NO NOT NUMERIC
                MOVE 'N' TO RECORD-ST.
           IF INS-LNAME = SPACES
                MOVE 'N' TO RECORD-ST.
           IF INS-FNAME = SPACES
                MOVE 'N' TO RECORD-ST.
           IF POLICY-BENEFIT-DATE-NUM NOT NUMERIC
                MOVE 'N' TO RECORD-ST.
           IF POLICY-AMOUNT NOT NUMERIC
                MOVE 'N' TO RECORD-ST.
           IF POLICY-DEDUCTIBLE-PAID NOT NUMERIC
                MOVE 'N' TO RECORD-ST.
           IF POLICY-COINSURANCE NOT NUMERIC
                MOVE 'N' TO RECORD-ST.
           IF CLAIM-AMOUNT NOT NUMERIC
                MOVE 'N' TO RECORD-ST.

       500-PRINT-RECORD.
           MOVE INS-POLICY-NO TO CON-POLICY-NO.

           EVALUATE POLICY-TYPE
                WHEN 1
                    MOVE 'EMPLOYER-PRIVATE'
                    TO CON-POLICY-TYPE
                WHEN 2
                    MOVE 'STANDARD MEDICARE'
                    TO CON-POLICY-TYPE
                WHEN 3
                    MOVE 'AFFORDABLE CARE ACT'
                    TO CON-POLICY-TYPE
           END-EVALUATE.

           MOVE INS-LNAME                TO CON-LNAME.
           MOVE INS-FNAME                TO CON-FNAME.
           MOVE POLICY-BENEFIT-DATE-X    TO CON-RENEW-DATE.
           MOVE DEDUCTIBLE-ST            TO CON-DEDUC-MET.
           MOVE DEDUC-PCTG               TO CON-COPAY-PCTG.
           MOVE DEDUCTIBLE-WS            TO CON-DEDUC-AMNT.
           MOVE CLAIMPAID-WS             TO CON-CLAIM-PAID.
           MOVE CLAIM-AMOUNT             TO CON-CLAIM-AMNT.

           WRITE PRINTFILE-REC FROM CONTENT-LINE.

       550-PRINT-HEADERS.
           WRITE PRINTFILE-REC FROM HEADER-LINE-01.
           MOVE SPACES TO PRINTFILE-REC.
           WRITE PRINTFILE-REC.
           WRITE PRINTFILE-REC FROM HEADER-LINE-02.
           WRITE PRINTFILE-REC FROM HEADER-LINE-03.
           WRITE PRINTFILE-REC FROM HEADER-LINE-04.

       900-CLEANUP.
           CLOSE PRINTFILE, CLAIMFILE.

       999-RETURN-ERR.
           GOBACK.



