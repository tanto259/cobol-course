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
           SELECT FAV-OUT ASSIGN TO FAVOUT.
           SELECT FAV-IN ASSIGN TO FAVIN.
      ****
       DATA DIVISION.
      ****
       FILE SECTION.
       FD  FAV-IN
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  FAV-REC.
           05  ARTIST-NAME                PIC X(30).
           05  NUMBER-OF-MUSICIANS        PIC 9(02).
           05  MUSICAL-GENRE              PIC X(12).
           05  COST.
                10  CD-COST               PIC 9(03)V99.
                10  SHIPPING-COST         PIC 9(02)V99.
                10  TAX                   PIC 9(02)V99.
           05  BAND-IS-STILL-TOGETHER     PIC X(01).
      *
       FD  FAV-OUT
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  FAVOUT-REC.
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
           05  BAND-IS-STILL-TOGETHER-O   PIC X(01).
      *
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05  LASTREC                    PIC X(01) VALUE SPACES.
      ****
       PROCEDURE DIVISION.
      ****
       OPEN-FILES.
           OPEN INPUT FAV-IN.
           OPEN OUTPUT FAV-OUT.
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           CLOSE FAV-OUT.
           CLOSE FAV-IN.
           GOBACK.
      *
       READ-RECORD.
           READ FAV-IN
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE ARTIST-NAME TO ARTIST-NAME-O.
           MOVE NUMBER-OF-MUSICIANS TO NUMBER-OF-MUSICIANS-O.
           MOVE MUSICAL-GENRE TO MUSICAL-GENRE-O.
           MOVE CD-COST TO CD-COST-O.
           MOVE SHIPPING-COST TO SHIPPING-COST-O.
           MOVE TAX TO TAX-O.
           MOVE BAND-IS-STILL-TOGETHER TO BAND-IS-STILL-TOGETHER-O.
           WRITE FAVOUT-REC.
