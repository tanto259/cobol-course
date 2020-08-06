       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPEDIT.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
           ASSIGN TO HOSPIN
             FILE STATUS IS IFCODE.

           SELECT RPTFILE
           ASSIGN TO RPTFILE
             FILE STATUS IS RFCODE.

           SELECT OUTFILE
           ASSIGN TO HOSPOUT
             FILE STATUS IS OFCODE.

           SELECT ERRFILE
           ASSIGN TO ERRFILE
             FILE STATUS IS EFCODE.

           SELECT INSTYPE
           ASSIGN TO INSTYPE
             FILE STATUS IS ISCODE.

      ******************************************************************

       DATA DIVISION.

      ******************************************************************
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS IN-REC.
       01  IN-REC            PIC X(100).

       FD  OUTFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUT-REC.
       01  OUT-REC           PIC X(133).

       FD  ERRFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ERR-REC.
       01  ERR-REC.
           05  ERR-DESC      PIC X(033).
           05  ERR-DATA      PIC X(100).

       FD  RPTFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS RPT-REC.
       01  RPT-REC           PIC X(133).

       FD  INSTYPE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS INS-REC.
       01  INS-REC.
           05  INS-TYPE-REC  PIC X(003).
           05  FILLER        PIC X(077).

      ******************************************************************
       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(02).
               88  CODE-READ                     VALUE SPACES.
               88  NO-MORE-DATA                  VALUE "10".
               88  IF-NORMAL                     VALUE "00".
           05  OFCODE                  PIC X(02).
               88  CODE-WRITE                    VALUE SPACES.
               88  OF-NORMAL                     VALUE "00".
           05  EFCODE                  PIC X(02).
               88  CODE-WRITE                    VALUE SPACES.
               88  EF-NORMAL                     VALUE "00".
           05  RFCODE                  PIC X(02).
               88  CODE-WRITE                    VALUE SPACES.
               88  RF-NORMAL                     VALUE "00".
           05  ISCODE                  PIC X(02).
               88  CODE-WRITE                    VALUE SPACES.
               88  IS-NORMAL                     VALUE "00".

       77  INS-COVERAGE-PERC           PIC 9(03) VALUE 10.

       01  WS-HEADER0-REC.
           05  FILLER                  PIC X(05) VALUE "PATNO".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(24) VALUE "PATIENT".
           05  FILLER                  PIC X(13) VALUE "PHONE-NO".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "TYPE".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "BED".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE "ADMIT".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(09) VALUE "DAILY AMT".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "DIAG".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "INS".
           05  FILLER                  PIC X(04) VALUE "STAY".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(07) VALUE "NETWORK".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE "COPAY".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE "DEDUCT".

       01  WS-HEADER1-REC.
           05  FILLER                  PIC X(05) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(22) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE ALL "=".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE ALL "=".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(09) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE ALL "=".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE ALL "=".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE ALL "=".

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(05).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PATIENT-NAME-O.
               10  PATIENT-LNAME-O     PIC X(10).
               10  FILLER              PIC X(02) VALUE SPACES.
               10  PATIENT-FNAME-O     PIC X(10).
               10  FILLER              PIC X(02) VALUE SPACES.
           05  PATIENT-PHONE-O.
               10  FILLER              PIC X(01) VALUE "(".
               10  PATIENT-AREACODE-O  PIC X(03).
               10  FILLER              PIC X(01) VALUE ")".
               10  PATIENT-PREFIX-O    PIC X(03).
               10  FILLER              PIC X(01) VALUE "-".
               10  PATIENT-SUFFIX-O    PIC X(04).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(02).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  BED-IDENTITY-O          PIC ZZZ9.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  CURR-DATE-O             PIC X(06).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 9(03).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  INS-TYPE-O              PIC X(04).
           05  HOSPITAL-STAY-LTH-O     PIC 9(03).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  NETWORK-O               PIC X(06).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  COPAY-O                 PIC $$,$$9.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  DEDUCT-O                PIC $$,$$9.

       01  WS-TOTALS-REC.
           05  WS-TOTALS-REC-01.
               10  FILLER                  PIC X(14)
                  VALUE "Rec in      : ".
               10  READ-OUT                PIC Z(11)9.
           05  WS-TOTALS-REC-02.
               10  FILLER                  PIC X(14)
                  VALUE "Rec written : ".
               10  WRITTEN-OUT             PIC Z(11)9.
           05  WS-TOTALS-REC-03.
               10  FILLER                  PIC X(14)
                  VALUE "Errors      : ".
               10  ERRORS-OUT              PIC Z(11)9.
           05  WS-TOTALS-REC-04.
               10  FILLER                  PIC X(14)
                  VALUE "Inpat       : ".
               10  INPATIENTS-OUT          PIC Z(11)9.
           05  WS-TOTALS-REC-05.
               10  FILLER                  PIC X(14)
                  VALUE "Outpat      : ".
               10  OUTPATIENTS-OUT         PIC Z(11)9.
           05  WS-TOTALS-REC-06.
               10  FILLER                  PIC X(14)
                  VALUE "HMO         : ".
               10  HMO-OUT                 PIC Z(11)9.
           05  WS-TOTALS-REC-07.
               10  FILLER                  PIC X(14)
                  VALUE "PPO         : ".
               10  PPO-OUT                 PIC Z(11)9.
           05  WS-TOTALS-REC-08.
               10  FILLER                  PIC X(14)
                  VALUE "MED         : ".
               10  MED-OUT                 PIC Z(11)9.
           05  WS-TOTALS-REC-09.
               10  FILLER                  PIC X(14)
                  VALUE "AFF         : ".
               10  AFF-OUT                 PIC Z(11)9.
           05  WS-TOTALS-REC-10.
               10  FILLER                  PIC X(14)
                  VALUE "PRI         : ".
               10  PRI-OUT                 PIC Z(11)9.
           05  WS-TOTALS-REC-11.
               10  FILLER                  PIC X(14)
                  VALUE "No Cov      : ".
               10  NO-COVERAGE-OUT         PIC Z(11)9.
           05  WS-TOTALS-REC-12.
               10  FILLER                  PIC X(14)
                  VALUE "GROSS       : ".
               10  TOTAL-GROSS-OUT         PIC $,$$$,$99.99.
           05  WS-TOTALS-REC-13.
               10  FILLER                  PIC X(14)
                  VALUE "NET         : ".
               10  TOTAL-NET-OUT           PIC $,$$$,$99.99.

       77  WS-DATE                     PIC 9(06).
       77  MORE-RECORDS-SW             PIC X(01) VALUE SPACE.
           88 NO-MORE-RECORDS                    VALUE 'N'.
       77  MORE-TYPE-SW                PIC X(01) VALUE SPACE.
           88 NO-MORE-TYPE                       VALUE 'N'.
       77  INS-TYPE-SW                 PIC X(01) VALUE SPACE.
           88 VALID-INS-TYPE                     VALUE 'Y'.
           88 INVALID-INS-TYPE                   VALUE 'N'.

       01  COUNTERS-AND-ACCUMULATORS.
           05 TYPE-READ                PIC S9(04) COMP.
           05 RECORDS-READ             PIC S9(04) COMP.
           05 RECORDS-WRITTEN          PIC S9(04) COMP.
           05 ERROR-RECS               PIC S9(04) COMP.
           05 NBR-INPATIENTS           PIC S9(04) COMP.
           05 NBR-OUTPATIENTS          PIC S9(04) COMP.
           05 NBR-HMO                  PIC S9(04) COMP.
           05 NBR-STATE-FED            PIC S9(04) COMP.
           05 NBR-NO-COVERAGE          PIC S9(04) COMP.
           05 NBR-PPO                  PIC S9(04) COMP.
           05 NBR-PRIVATE              PIC S9(04) COMP.
           05 NBR-AFFORDABLE           PIC S9(04) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(07)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(07)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(07)V99 COMP-3.

       COPY PATREC.

       01  INS-TYPE-TABLE.
           05  INS-TYPE-ITEM OCCURS 5 TIMES INDEXED BY T-IDX PIC X(03).
                88  HMO VALUE 'HMO'.
                88  PRI VALUE 'PRI'.
                88  PPO VALUE 'PPO'.
                88  AFF VALUE 'AFF'.
                88  MED VALUE 'MED'.

      ******************************************************************

       PROCEDURE DIVISION.

      **** Perform the paragraphs, when done return to caller
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

      ******************************************************************
      **** HOUSEKEEPING
      **** > OPEN FILE
      **** > INITIALIZE WS RECORDS
      **** > READ FIRST DATA
      **** > MOVE TYPE FROM FILE TO INTERNAL TABLE

       000-HOUSEKEEPING.

      **** Read current date
           ACCEPT WS-DATE FROM DATE.

      **** Initialize the counters and other working storage records
           INITIALIZE  COUNTERS-AND-ACCUMULATORS,
                       WS-OUTPUT-REC,
                       WS-TOTALS-REC

      **** Perform the open file paragraph
           PERFORM 025-OPEN-FILES THRU 025-EXIT.

      **** Read the first data, terminate if no data
           READ INFILE INTO WS-INPUT-REC
               AT END
               MOVE 'N' TO MORE-RECORDS-SW
               GO TO 000-EXIT
           END-READ.

           ADD +1 TO RECORDS-READ.

      **** Perform the read type and print headers paragraph
           PERFORM 050-READ-TYPE-RECORDS THRU 050-EXIT.
           PERFORM 075-PRINT-HEADERS THRU 075-EXIT.

       000-EXIT.
           EXIT.

      ******************************************************************

       025-OPEN-FILES.

      **** Open input and output files, if not normal, terminate.
           OPEN INPUT INFILE
           IF NOT IF-NORMAL
               DISPLAY "ERROR OPENING INPUT PATIENT RECORDS"
               GO TO 999-RETURN-ERR.

           OPEN INPUT INSTYPE
           IF NOT IS-NORMAL
               DISPLAY "ERROR OPENING INPUT TYPE RECORDS"
               GO TO 999-RETURN-ERR.

           OPEN OUTPUT OUTFILE
           IF NOT OF-NORMAL
               DISPLAY "ERROR OPENING OUTPUT OUTFILE"
               GO TO 999-RETURN-ERR.

           OPEN OUTPUT RPTFILE
           IF NOT RF-NORMAL
               DISPLAY "ERROR OPENING OUTPUT RPTFILE"
               GO TO 999-RETURN-ERR.

           OPEN OUTPUT ERRFILE
           IF NOT EF-NORMAL
               DISPLAY "ERROR OPENING OUTPUT ERRFILE"
               GO TO 999-RETURN-ERR.

       025-EXIT.
           EXIT.

      ******************************************************************

       050-READ-TYPE-RECORDS.

      **** Read the insurance type records, terminate if no data
           READ INSTYPE
               AT END
               MOVE 'N' TO MORE-TYPE-SW
               GO TO 050-EXIT
           END-READ.

      **** Use perform varying to retrieve data from file and put them
      **** inside a table, read the next line, terminate if no more data
           PERFORM VARYING T-IDX FROM 1 BY 1 UNTIL NO-MORE-TYPE
               MOVE INS-TYPE-REC TO INS-TYPE-ITEM (T-IDX)
               ADD +1 TO TYPE-READ
               READ INSTYPE
                   AT END
                   MOVE 'N' TO MORE-TYPE-SW
                   GO TO 050-EXIT
               END-READ
           END-PERFORM.

       050-EXIT.
           EXIT.

      ******************************************************************

       075-PRINT-HEADERS.

      **** Write headers to report
           WRITE RPT-REC FROM WS-HEADER0-REC.
           WRITE RPT-REC FROM WS-HEADER1-REC.

       075-EXIT.
           EXIT.

      ******************************************************************
      **** MAINLINE
      **** > VALIDATE PATIENTS
      **** > CALCULATE THE STATISTICS
      **** > WRITE RECORDS TO REPORT

       100-MAINLINE.

      **** Perform the paragraphs
           PERFORM 110-VALIDATE-PATIENTS THRU 110-EXIT.
           PERFORM 120-CALCULATE-STATS THRU 120-EXIT.
           PERFORM 130-WRITE-RECORDS THRU 130-EXIT.

       100-EXIT.
           EXIT.

      ******************************************************************

       110-VALIDATE-PATIENTS.

      **** Use search to see if the insurance type is valid or not
           SET T-IDX TO 1.
           SEARCH INS-TYPE-ITEM
           AT END
               MOVE 'N' TO INS-TYPE-SW
           WHEN INS-TYPE-ITEM (T-IDX) = INS-TYPE
               MOVE 'Y' TO INS-TYPE-SW.

      **** If type and insurance type is invalid, print data to out-rec.
      **** Then, move reason to error report and read for another line.
      **** If no more line, terminate the whole 100-MAINLINE paragraph.
           IF NOT (VALID-TYPE AND VALID-INS-TYPE)

               MOVE WS-INPUT-REC TO OUT-REC
               WRITE OUT-REC

               IF VALID-TYPE
                   MOVE "INVALID INS TYPE" TO ERR-DESC
               ELSE
                   MOVE "INVALID PAT TYPE" TO ERR-DESC
               END-IF

               MOVE WS-INPUT-REC TO ERR-DATA

               WRITE ERR-REC
               ADD +1 TO ERROR-RECS

               PERFORM 900-READ-PATIENTS-RECORDS THRU 900-EXIT
               ADD +1 TO RECORDS-READ

               GO TO 100-EXIT
           END-IF.

       110-EXIT.
           EXIT.

      ******************************************************************

       120-CALCULATE-STATS.

      **** Add +1 to the insurance type counter which correspond to the
      **** patient's insurance
           IF HMO(T-IDX)
               ADD +1 TO NBR-HMO
           ELSE IF MED(T-IDX)
               ADD +1 TO NBR-STATE-FED
           ELSE IF AFF(T-IDX)
               ADD +1 TO NBR-AFFORDABLE
           ELSE IF PPO(T-IDX)
               ADD +1 TO NBR-PPO
           ELSE IF PRI(T-IDX)
               ADD +1 TO NBR-PRIVATE
           ELSE
               ADD +1 TO NBR-NO-COVERAGE
           END-IF.

      **** Add +1 depending on the patient's type
           IF INPATIENT
               ADD +1 TO NBR-INPATIENTS
           ELSE
               ADD +1 TO NBR-OUTPATIENTS
           END-IF.

      **** Compute the total net amount of the patient.
           COMPUTE PAT-TOTAL-AMT-NET =
               (PATIENT-TOT-AMT  +
                   AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / 100))
           END-COMPUTE.

      **** Add the gross and net amount to the end counter.
           ADD PAT-TOTAL-AMT-NET   TO TOTAL-AMT-NET.
           ADD PATIENT-TOT-AMT     TO TOTAL-AMT-GROSS.

       120-EXIT.
           EXIT.

      ******************************************************************

       130-WRITE-RECORDS.

      **** Move the corresponding data to the output storage
           MOVE PATIENT-NBR        TO PATIENT-NBR-O.
           MOVE LAST-NAME          TO PATIENT-LNAME-O.
           MOVE FIRST-NAME         TO PATIENT-FNAME-O.
           MOVE PATIENT-PHONE(1:3) TO PATIENT-AREACODE-O.
           MOVE PATIENT-PHONE(4:3) TO PATIENT-PREFIX-O.
           MOVE PATIENT-PHONE(7:4) TO PATIENT-SUFFIX-O.
           MOVE PATIENT-TYPE       TO PATIENT-TYPE-O.
           MOVE WS-DATE            TO CURR-DATE-O.
           MOVE BED-IDENTITY       TO BED-IDENTITY-O.
           ADD  PAT-TOTAL-AMT-NET  TO PATIENT-TOT-AMT
                                   GIVING PATIENT-AMT-PER-DAY-O.
           MOVE INS-COVERAGE-PERC  TO INS-COVERAGE-PERC-O.
           MOVE INS-TYPE           TO INS-TYPE-O.
           ADD  +1                 TO HOSPITAL-STAY-LTH
                                   GIVING  HOSPITAL-STAY-LTH-O.
           MOVE COPAY              TO COPAY-O.
           MOVE DEDUCTIBLE         TO DEDUCT-O.

           IF IN-NETWORK
               MOVE "IN" TO NETWORK-O
           ELSE IF OUT-OF-NETWORK
               MOVE "OUT" TO NETWORK-O
           END-IF.

      **** Write the record to the output report.
           WRITE RPT-REC FROM WS-OUTPUT-REC.
           ADD +1 TO RECORDS-WRITTEN.

      **** Read the next data, terminate if no more data.
           PERFORM 900-READ-PATIENTS-RECORDS THRU 900-EXIT.
           ADD +1 TO RECORDS-READ.

       130-EXIT.
           EXIT.

      ******************************************************************
      **** CLEAN-UP SECTION
      **** > CLOSE FILES AND PRINT FOOTER

       200-CLEANUP.

      **** Perform the print footer paragraph then close files
           PERFORM 210-PRINT-FOOTER THRU 210-EXIT.
           CLOSE OUTFILE, RPTFILE, ERRFILE, INFILE.

       200-EXIT.
           EXIT.

      ******************************************************************

       210-PRINT-FOOTER.

      **** Move the corresponding stats data to the output storage
           MOVE RECORDS-READ            TO READ-OUT.
           MOVE RECORDS-WRITTEN         TO WRITTEN-OUT.
           MOVE ERROR-RECS              TO ERRORS-OUT.
           MOVE NBR-INPATIENTS          TO INPATIENTS-OUT.
           MOVE NBR-OUTPATIENTS         TO OUTPATIENTS-OUT.
           MOVE NBR-HMO                 TO HMO-OUT.
           MOVE NBR-STATE-FED           TO MED-OUT.
           MOVE NBR-AFFORDABLE          TO AFF-OUT.
           MOVE NBR-PPO                 TO PPO-OUT.
           MOVE NBR-PRIVATE             TO PRI-OUT.
           MOVE NBR-NO-COVERAGE         TO NO-COVERAGE-OUT.
           MOVE TOTAL-AMT-GROSS         TO TOTAL-GROSS-OUT.
           MOVE TOTAL-AMT-NET           TO TOTAL-NET-OUT.

      **** Write a blank line as spacing
           MOVE SPACES TO RPT-REC.
           WRITE RPT-REC.

      **** Write the common stats data
           WRITE RPT-REC FROM WS-TOTALS-REC-01.
           WRITE RPT-REC FROM WS-TOTALS-REC-02.
           WRITE RPT-REC FROM WS-TOTALS-REC-03.
           WRITE RPT-REC FROM WS-TOTALS-REC-04.
           WRITE RPT-REC FROM WS-TOTALS-REC-05.
           WRITE RPT-REC FROM WS-TOTALS-REC-06.
           WRITE RPT-REC FROM WS-TOTALS-REC-07.
           WRITE RPT-REC FROM WS-TOTALS-REC-08.
           WRITE RPT-REC FROM WS-TOTALS-REC-09.
           WRITE RPT-REC FROM WS-TOTALS-REC-10.
           WRITE RPT-REC FROM WS-TOTALS-REC-11.

      **** Write a blank line as spacing
           MOVE SPACES TO RPT-REC.
           WRITE RPT-REC.

      **** Write the net and gross amount total
           WRITE RPT-REC FROM WS-TOTALS-REC-12.
           WRITE RPT-REC FROM WS-TOTALS-REC-13.

       210-EXIT.
           EXIT.

      ******************************************************************

       900-READ-PATIENTS-RECORDS.

      **** Read patients data, if no records, terminate 100-MAINLINE.
      **** This is intended for those used within 100-MAINLINE.
           READ INFILE INTO WS-INPUT-REC
               AT END MOVE "N" TO MORE-RECORDS-SW
               GO TO 100-EXIT
           END-READ.

       900-EXIT.
           EXIT.

      ******************************************************************

       999-RETURN-ERR.
           GOBACK.



