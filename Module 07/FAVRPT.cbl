       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      *******************************************************************
      * This program display a report about some music groups           *
      *******************************************************************
      ****
       ENVIRONMENT DIVISION.
      ****
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAV-OUT
           ASSIGN TO FAVOUT
           ACCESS MODE IS SEQUENTIAL.

           SELECT FAV-IN
           ASSIGN TO FAVIN
           ORGANIZATION IS SEQUENTIAL.
      ****
       DATA DIVISION.
      ****
       FILE SECTION.
       FD  FAV-IN
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  FAV-REC                        PIC X(80).

      *
       FD  FAV-OUT
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  FAVOUT-REC                     PIC X(80).
      ****
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05  LASTREC                    PIC X(01) VALUE SPACES.
      *
       01  FAVREC-IN.
           05  ARTIST-NAME                PIC X(30).
           05  NUMBER-OF-MUSICIANS        PIC 9(02).
           05  MUSICAL-GENRE              PIC X(12).
           05  COST.
                10  CD-COST               PIC 9(03)V99.
                10  SHIPPING-COST         PIC 9(02)V99.
                10  TAX                   PIC 9(02)V99.
           05  BAND-IS-STILL-TOGETHER     PIC X(01).
      *
       01  FAVREC-OUT.
           05  ARTIST-NAME-O              PIC X(30).
           05  FILLER                     PIC X(01) VALUE SPACES.
           05  NUMBER-OF-MUSICIANS-O      PIC 9(02).
           05  FILLER                     PIC X(01) VALUE SPACES.
           05  MUSICAL-GENRE-O            PIC X(12).
           05  FILLER                     PIC X(01) VALUE SPACES.
           05  COST-O.
                10  CD-COST-O             PIC $ZZ9.99.
                10  FILLER                PIC X(01) VALUE SPACES.
                10  SHIPPING-COST-O       PIC $Z9.99.
                10  FILLER                PIC X(01) VALUE SPACES.
                10  TAX-O                 PIC $Z9.99.
                10  FILLER                PIC X(01) VALUE SPACES.
                10  TOTAL-COST-O          PIC $ZZZ9.99.
                10  FILLER                PIC X(01) VALUE SPACES.
           05  BAND-IS-STILL-TOGETHER-O   PIC X(01).
      *
       77  TOTAL-COST                     PIC 9(4)V99 VALUE ZERO.
       77  REC-KTR                        PIC 99 VALUE ZERO.
       77  GRAND-TOTAL                    PIC 9(5)V99 VALUE ZERO.
       77  AVG-SALE                       PIC 9(4)V99 VALUE ZERO.
       77  HIGHEST-COST                   PIC 9(4)V99 VALUE ZERO.
       77  LOWEST-COST                    PIC 9(4)V99 VALUE 9999.99.
      *
       01  RECORD-NUM.
           05  FILLER                     PIC X(20) VALUE
               'Number of records: '.
           05  REC-KTR-OUT                PIC Z9.
      *
       01  GRAND-TOTAL-SALES.
           05  FILLER                     PIC X(20) VALUE
               'Gross revenue: '.
           05  COST-TOTAL-OUT             PIC $ZZZZ9.99.
      *
       01  AVERAGE-TOTAL-SALES.
           05  FILLER                     PIC X(20) VALUE
               'Average CD sale: '.
           05  AVG-SALE-O                 PIC $ZZZ9.99.
      *
       01  LOWEST-CD-COST.
           05  FILLER                     PIC X(20) VALUE
               'Lowest CD cost: '.
           05  LOWEST-COST-O              PIC $ZZZ9.99.
      *
       01  HIGHEST-CD-COST.
           05  FILLER                     PIC X(20) VALUE
               'Highest CD cost: '.
           05  HIGHEST-COST-O             PIC $ZZZ9.99.
      ****
       PROCEDURE DIVISION.
      ****
       MAIN.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL LASTREC = 'Y'.
           PERFORM 600-Closing-Statistics.
           PERFORM 700-Close-Files.
           GOBACK.
      *
       000-Housekeeping.
           INITIALIZE FAVREC-IN, FAVREC-OUT.
           PERFORM 300-Open-Files.
           PERFORM 400-Read-Record.
      *
       100-Main.
           PERFORM 200-Process-Data.
           PERFORM 500-Write-Record.
           PERFORM 400-Read-Record.
      *
       200-Process-Data.
           MOVE ARTIST-NAME            TO ARTIST-NAME-O.
           MOVE NUMBER-OF-MUSICIANS    TO NUMBER-OF-MUSICIANS-O.
           MOVE MUSICAL-GENRE          TO MUSICAL-GENRE-O.
           MOVE CD-COST                TO CD-COST-O.
           MOVE SHIPPING-COST          TO SHIPPING-COST-O.
           MOVE TAX                    TO TAX-O.
           MOVE BAND-IS-STILL-TOGETHER TO BAND-IS-STILL-TOGETHER-O.
           COMPUTE TOTAL-COST = CD-COST + TAX + SHIPPING-COST.
           MOVE TOTAL-COST TO TOTAL-COST-O.
           ADD TOTAL-COST TO GRAND-TOTAL.
           ADD 1 TO REC-KTR.
           IF TOTAL-COST < LOWEST-COST
               MOVE TOTAL-COST TO LOWEST-COST.
           IF TOTAL-COST > HIGHEST-COST
               MOVE TOTAL-COST TO HIGHEST-COST.
      *
       300-Open-Files.
           OPEN INPUT FAV-IN.
           OPEN OUTPUT FAV-OUT.
      *
       400-Read-Record.
           READ FAV-IN INTO FAVREC-IN
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       500-Write-Record.
           WRITE FAVOUT-REC FROM FAVREC-OUT.
      *
       600-Closing-Statistics.
           MOVE GRAND-TOTAL TO COST-TOTAL-OUT.
           MOVE REC-KTR TO REC-KTR-OUT.
           COMPUTE AVG-SALE = GRAND-TOTAL / REC-KTR.
           MOVE AVG-SALE TO AVG-SALE-O.
           MOVE HIGHEST-COST TO HIGHEST-COST-O.
           MOVE LOWEST-COST TO LOWEST-COST-O.
           WRITE FAVOUT-REC FROM RECORD-NUM.
           WRITE FAVOUT-REC FROM GRAND-TOTAL-SALES.
           WRITE FAVOUT-REC FROM AVERAGE-TOTAL-SALES.
           WRITE FAVOUT-REC FROM LOWEST-CD-COST.
           WRITE FAVOUT-REC FROM HIGHEST-CD-COST.
      *
       700-Close-Files.
           CLOSE FAV-IN, FAV-OUT.

