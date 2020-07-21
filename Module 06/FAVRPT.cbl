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
           05  FILLER                     PIC X(02) VALUE SPACES.
           05  NUMBER-OF-MUSICIANS-O      PIC 9(02).
           05  FILLER                     PIC X(02) VALUE SPACES.
           05  MUSICAL-GENRE-O            PIC X(12).
           05  FILLER                     PIC X(02) VALUE SPACES.
           05  COST-O.
                10  CD-COST-O             PIC 9(03)V99.
                10  FILLER                PIC X(02) VALUE SPACES.
                10  SHIPPING-COST-O       PIC 9(02)V99.
                10  FILLER                PIC X(02) VALUE SPACES.
                10  TAX-O                 PIC 9(02)V99.
                10  FILLER                PIC X(02) VALUE SPACES.
                10  TOTAL-COST-O          PIC 9(04)V99.
                10  FILLER                PIC X(02) VALUE SPACES.
           05  BAND-IS-STILL-TOGETHER-O   PIC X(01).
      ****
       PROCEDURE DIVISION.
      ****
       MAIN.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL LASTREC = 'Y'.
           PERFORM 600-Close-Files.
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
           COMPUTE TOTAL-COST-O = CD-COST + TAX + SHIPPING-COST.
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
       600-Close-Files.
           CLOSE FAV-IN, FAV-OUT.

