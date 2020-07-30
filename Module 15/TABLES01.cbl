       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPROJ.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  EMP-PROJECT-TABLE-I.
           05 EMP-PROJECT-I                 PIC X(4).
           05 EMP-NAME-I                    PIC X(15).
           05 EMP-STATE-OFFICE-I            PIC X(02).
           05 EMP-PROJECT-POSITION-I        PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ-I        PIC 9(03).
           05 EMP-NBR-OT-HOURS-I            PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE-I    PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE-I        PIC 9(03)V99.
           05 EMP-LANGUAGE-CERT-I           PIC X(20).
           05 EMP-ON-CALL-I                 PIC X(01).
           05 FILLER                        PIC X(02).
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.
       01  EMP-PROJECT-TABLE.
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES
                ASCENDING KEY IS EMP-NAME
                INDEXED BY PROJ-IDX.
                10 EMP-PROJECT               PIC X(4).
                10 EMP-NAME                  PIC X(15).
                10 EMP-STATE-OFFICE          PIC X(02).
                10 EMP-PROJECT-POSITION      PIC X(20).
                10 EMP-NBR-DAYS-ON-PROJ      PIC 9(03).
                10 EMP-NBR-OT-HOURS          PIC 9(03).
                10 EMP-PER-DAY-BILLING-RATE  PIC 9(03)V99.
                10 EMP-PER-HOUR-OT-RATE      PIC 9(03)V99.
                10 EMP-DAY-BILL              PIC 9(06)V99.
                10 EMP-OT-BILL               PIC 9(06)V99.
                10 EMP-LANGUAGE-CERT         PIC X(20).
                10 EMP-ON-CALL               PIC X(01).
                10 FILLER                    PIC X(02).
       01  TEMP-CALC-VALUE.
           05  TEMP-SUMMARY     PIC 9(16)V99 VALUE 0.
           05  TEMP-SUMMARY-O   PIC $,$$$,$$$,$$$,$$$,$$9.99.
           05  TEMP-BILL-RATE   PIC 9(03)V99.
       77  TEMP-CTR             PIC 9(4).
       77  TEMP-NAME            PIC X(15).
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE EMP-PROJECT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL PROJECT-INDEX > TABLE-MAX *> Load Table
           OR END-OF-FILE
                MOVE EMP-PROJECT-I TO
                        EMP-PROJECT (PROJECT-INDEX)
                MOVE EMP-NAME-I TO
                        EMP-NAME (PROJECT-INDEX)
                MOVE EMP-STATE-OFFICE-I TO
                        EMP-STATE-OFFICE  (PROJECT-INDEX)
                MOVE EMP-PROJECT-POSITION-I TO
                        EMP-PROJECT-POSITION  (PROJECT-INDEX)
                MOVE EMP-NBR-DAYS-ON-PROJ-I TO
                        EMP-NBR-DAYS-ON-PROJ (PROJECT-INDEX)
                MOVE EMP-NBR-OT-HOURS-I  TO
                        EMP-NBR-OT-HOURS (PROJECT-INDEX)
                MOVE EMP-PER-DAY-BILLING-RATE-I TO
                        EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX)
                MOVE EMP-PER-HOUR-OT-RATE-I  TO
                        EMP-PER-HOUR-OT-RATE (PROJECT-INDEX)
                MOVE EMP-LANGUAGE-CERT-I  TO
                        EMP-LANGUAGE-CERT (PROJECT-INDEX)
                MOVE EMP-ON-CALL-I   TO
                        EMP-ON-CALL (PROJECT-INDEX)
                MULTIPLY EMP-NBR-DAYS-ON-PROJ (PROJECT-INDEX) BY
                   EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX)
                   GIVING EMP-DAY-BILL (PROJECT-INDEX)
                MULTIPLY EMP-NBR-OT-HOURS (PROJECT-INDEX) BY
                   EMP-PER-HOUR-OT-RATE (PROJECT-INDEX)
                   GIVING EMP-OT-BILL (PROJECT-INDEX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
                DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
           END-PERFORM.
           DISPLAY " ".
       100-PROCESS-TABLE-DATA.
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.
           PERFORM 500-TOTAL-ALL-PROJECTS-EXPENSE.
           PERFORM 610-SELECT-COUNT-FROM-WHERE.
           PERFORM 620-SELECT-FROM-WHERE.
           PERFORM 630-SELECT-FROM-WHERE-WILDCARD.
           PERFORM 640-SELECT-FROM-WHERE-SUBQUERY.
           PERFORM 650-SELECT-FROM-WHERE-TF.
       200-FIND-PROJECT.
      ***  Display all of the Employee names working on project 'A111'
           DISPLAY "EMPLOYEE WORKING ON A111".
           DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PROJECT (PROJECT-INDEX) = 'A111'
                   DISPLAY EMP-NAME (PROJECT-INDEX)
               END-IF
           END-PERFORM.
           DISPLAY " ".
       300-FIND-NC-OT-SKILL.
      ***  Display all of the Employee names of Programmers in NC
      ***     who are allowed to bill for On-Call work
           DISPLAY "EMPLOYEE IN NC ALLOWED TO BILL FOR ON-CALL".
           DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-STATE-OFFICE (PROJECT-INDEX) = 'NC' AND
                 EMP-ON-CALL (PROJECT-INDEX) = 'Y' THEN
                   DISPLAY EMP-NAME (PROJECT-INDEX)
               END-IF
           END-PERFORM.
           DISPLAY " ".
       400-TOTAL-PROJ-EXPENSE.
      ***  Calculate and Display the total cost for the 'A111' project
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PROJECT (PROJECT-INDEX) = 'A111'
                   COMPUTE TEMP-SUMMARY = TEMP-SUMMARY
                       + EMP-DAY-BILL (PROJECT-INDEX)
                       + EMP-OT-BILL (PROJECT-INDEX)
               END-IF
           END-PERFORM.
           MOVE TEMP-SUMMARY TO TEMP-SUMMARY-O.
           DISPLAY "TOTAL COST FOR A111 : " TEMP-SUMMARY-O.
           DISPLAY " ".
       500-TOTAL-ALL-PROJECTS-EXPENSE.
      ***  Calculate & Display the total cost for all of the projects
      **   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL))
           COMPUTE TEMP-SUMMARY = FUNCTION SUM(EMP-DAY-BILL(ALL)) +
               FUNCTION SUM(EMP-OT-BILL(ALL)).
           MOVE TEMP-SUMMARY TO TEMP-SUMMARY-O.
           DISPLAY "TOTAL COST FOR ALL  : " TEMP-SUMMARY-O.
           DISPLAY " ".
       610-SELECT-COUNT-FROM-WHERE.
      ***  Calculate the amount of programmer/analyst on 'A111' from NC
      ***  Equiv: SELECT COUNT(*) FROM TABLE WHERE PROJECT = 'A111'
      ***  AND STATE = 'NC' AND POSITION = 'PROGRAMMER/ANALYST'
           MOVE 0 TO TEMP-CTR.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PROJECT (PROJECT-INDEX) = 'A111' AND
                 EMP-STATE-OFFICE (PROJECT-INDEX) = 'NC' AND
                 EMP-PROJECT-POSITION (PROJECT-INDEX) =
                 'PROGRAMMER/ANALYST'
                   ADD 1 TO TEMP-CTR
               END-IF
            END-PERFORM.
            DISPLAY "AMOUNT OF NC PROGRAMMER/ANALYST @ A111: " TEMP-CTR.
            DISPLAY " ".
       620-SELECT-FROM-WHERE.
      ***  Display the DBA on project B222
      ***  Equiv: SELECT NAME FROM TABLE WHERE PROJECT = 'B222' AND
      ***  POSITION = 'DATABASE ADMIN'
           DISPLAY "DBA WORKING ON B222".
           DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PROJECT (PROJECT-INDEX) = 'B222' AND
                 EMP-PROJECT-POSITION (PROJECT-INDEX) =
                 'DATABASE ADMIN'
                   DISPLAY EMP-NAME (PROJECT-INDEX)
               END-IF
           END-PERFORM.
           DISPLAY " ".
       630-SELECT-FROM-WHERE-WILDCARD.
      ***  Return all employee whose project ends with 333
      ***  Equiv: SELECT NAME FROM TABLE WHERE PROJECT = "%333"
           DISPLAY "EMPLOYEE WHOSE PROJECT IS %333".
           DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PROJECT (PROJECT-INDEX)(2:3) = '333'
                   DISPLAY EMP-NAME (PROJECT-INDEX)
               END-IF
            END-PERFORM.
            DISPLAY " ".
       640-SELECT-FROM-WHERE-SUBQUERY.
      ***  Return the highest daily rate programmer/analyst
      ***  Equiv: SELECT NAME FROM TABLE WHERE SALARY IN (SELECT
      ***  MAX(SALARY) FROM TABLE)
           COMPUTE TEMP-BILL-RATE =
             FUNCTION MAX(EMP-PER-DAY-BILLING-RATE(ALL)).
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX) =
                 TEMP-BILL-RATE AND EMP-PROJECT-POSITION (PROJECT-INDEX)
                 = 'PROGRAMMER/ANALYST'
                 MOVE EMP-NAME (PROJECT-INDEX) TO TEMP-NAME
               END-IF
           END-PERFORM.
           DISPLAY "HIGHEST DAILY RATE PROGRAMMER/ANALYST : " TEMP-NAME.
           DISPLAY " ".
       650-SELECT-FROM-WHERE-TF.
      ***  Can Kandace Springs do COBOL?
      ***  Equiv: if (SELECT LANGCERT FROM TABLE WHERE NAME = 'Kandace
      ***  Springs') = 'COBOL' ? T : F
           DISPLAY "CAN KANDACE SPRINGS DO COBOL?"
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX
               IF EMP-NAME (PROJECT-INDEX) = 'KANDACE SPRINGS'
                   IF EMP-LANGUAGE-CERT (PROJECT-INDEX) = 'COBOL' THEN
                       DISPLAY "YES"
                   ELSE
                       DISPLAY "NO"
                   END-IF
               END-IF
           END-PERFORM.
           DISPLAY " ".
       900-WRAP-UP.
           CLOSE INPUT-FILE.