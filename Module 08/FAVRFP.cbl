       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
      *******************************************************************
      * This program display a report about some music groups           *
      *******************************************************************
      ****
       ENVIRONMENT DIVISION.
      ****
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFP-OUT
           ASSIGN TO PROPOSAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT IVL-OUT
           ASSIGN TO INVLID
           ACCESS MODE IS SEQUENTIAL.

           SELECT RFP-IN
           ASSIGN TO RFPIN
           ORGANIZATION IS SEQUENTIAL.
      ****
       DATA DIVISION.
      ****
       FILE SECTION.
       FD  RFP-IN
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  RFPIN-REC                        PIC X(80).
      *
       FD  IVL-OUT
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS.
       01  IVL-REC                          PIC X(80).
      *
       FD  RFP-OUT
           RECORDING MODE F
           RECORD CONTAINS 133 CHARACTERS.
       01  RFPOUT-REC                     PIC X(133).
      ****
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05  LASTREC                     PIC X(01) VALUE SPACES.
           05  ACCT-NO                     PIC X(01) VALUE 'N'.
                88 ACCT-VALID               VALUE 'Y'.
           05  GENRE                       PIC X(01) VALUE 'N'.
                88 GENRE-VALID              VALUE 'Y'.
           05  NAMES                       PIC X(01) VALUE 'N'.
                88 NAME-VALID               VALUE 'Y'.
           05  INST-TYPE                   PIC X(01) VALUE 'N'.
                88 TYPE-VALID               VALUE 'Y'.
           05  INST-QUAL                   PIC X(01) VALUE 'N'.
                88 QUAL-VALID               VALUE 'Y'.
           05  MAX-BUDGET                  PIC X(01) VALUE 'N'.
                88 BUDG-VALID               VALUE 'Y'.
           05  SHIP-COND                   PIC X(01) VALUE 'N'.
                88 COND-VALID               VALUE 'Y'.
      *
       01  RFP-REC.
           05  ARTIST-ACCT-NO                   PIC X(08).
           05  ARTIST-MUSICAL-GENRE             PIC X(09).
                88  ROCK        VALUE   'ROCK'.
                88  JAZZ        VALUE   'JAZZ'.
                88  FUSION      VALUE 'FUSION'.
                88  FOLK        VALUE 'FOLK'.
                88  CLASSICAL   VALUE 'CLASSICAL'.
                88  COUNTRY     VALUE 'COUNTRY'.
           05  MUSICIAN.
                10  MUSICIAN-LNAME              PIC X(15).
                10  MUSICIAN-FNAME              PIC X(15).
           05  MUSICIAN-INSTRUMENT-TYPE         PIC X(06).
                88  KEYBOARD        VALUE 'KEYS'.
                88  VOCALS          VALUE 'VOCALS'.
                88  GUITAR          VALUE 'GUITAR'.
                88  BASS            VALUE 'BASS'.
                88  DRUMS           VALUE 'DRUMS'.
                88  PERCUSSION      VALUE 'PERC'.
           05  INSTRUMENT-QUALITY               PIC X(01).
                88  USED-FLAG       VALUE   "U".
                88  NEW-FLAG        VALUE   "N".
                88  PREMIUM-FLAG    VALUE   "P".
           05  MAX-MUSICIAN-BUDGET-AMOUNT       PIC 9(05)V99.
           05  SHIP-TO                          PIC X(03).
                88  IN-COUNTRY      VALUE   "IN".
                88  OUT-OF-COUNTRY  VALUE   "OUT".
           05  FILLER                           PIC X(16).
      *
       01  PROP-REC.
           05  ARTIST-ACCT-NO-O                 PIC X(08).
           05  FILLER                           PIC X(02) VALUE SPACES.
           05  ARTIST-MUSICAL-GENRE-O           PIC X(09).
           05  FILLER                           PIC X(02) VALUE SPACES.
           05  MUSICIAN-O.
                10  MUSICIAN-LNAME-O            PIC X(15).
                10  FILLER                      PIC X(01) VALUE SPACES.
                10  MUSICIAN-FNAME-O            PIC X(15).
           05  FILLER                           PIC X(02) VALUE SPACES.
           05  MUSICIAN-INSTRUMENT-TYPE-O       PIC X(12).
           05  FILLER                           PIC X(01) VALUE SPACES.
           05  INSTRUMENT-QUALITY-O             PIC X(10).
           05  FILLER                           PIC X(01) VALUE SPACES.
           05  SHIP-TO-O                        PIC X(03).
           05  FILLER                           PIC X(02) VALUE SPACES.
           05  COST-PER-INSTRUMENT-O            PIC $,$$$,$$9.99.
           05  FILLER                           PIC X(02) VALUE SPACES.
           05  ADDITIONAL-COSTS-O.
                10  SHIPPING-COST-O             PIC $,$$9.99.
                10  FILLER                      PIC X(02) VALUE SPACES.
                10  TAX-O                       PIC $$9.99.
                10  FILLER                      PIC X(02) VALUE SPACES.
           05  TOTAL-O                          PIC $$,$$$,$$9.99.
           05  FILLER                           PIC X(01).
      *
       01  HEADER-REC-0.
           05  FILLER            PIC X(20) VALUE "REQUEST FOR PROPOSAL".
       01  HEADER-REC-1.
           05  FILLER            PIC X(20) VALUE "====================".
       01  HEADER-REC-2.
           05  FILLER            PIC X(20) VALUE SPACES.
      *
       01  HEADER-REC-3.
           05  FILLER                  PIC X(08) VALUE "ACCT NO".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(09) VALUE "GENRE".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  MUSICIAN-H.
                10  FILLER             PIC X(15) VALUE "LAST NAME".
                10  FILLER             PIC X(01) VALUE SPACES.
                10  FILLER             PIC X(15) VALUE "FIRST NAME".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE "TYPE".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE "QUALITY".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "SHIP".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE "COST".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  ADDITIONAL-COSTS-H.
                10  FILLER             PIC X(08) VALUE "SHPCOST".
                10  FILLER             PIC X(01) VALUE SPACES.
                10  FILLER             PIC X(07) VALUE "TAX".
                10  FILLER             PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE "TOTAL".
           05  FILLER                  PIC X(01).
      *
       01  HEADER-REC-4.
           05  FILLER                  PIC X(08) VALUE "--------".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(09) VALUE "---------".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  MUSICIAN-H.
                10  FILLER            PIC X(15) VALUE "---------------".
                10  FILLER             PIC X(01) VALUE SPACES.
                10  FILLER            PIC X(15) VALUE "---------------".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE "------------".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE "----------".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE "----".
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE "------------".
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  ADDITIONAL-COSTS-H.
                10  FILLER             PIC X(08) VALUE "--------".
                10  FILLER             PIC X(01) VALUE SPACES.
                10  FILLER             PIC X(07) VALUE "-------".
                10  FILLER             PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE "-------------".
           05  FILLER                  PIC X(01).
      *
       01  FOOTER-REC-0.
           05  FILLER                  PIC X(20) VALUE SPACES.
       01  FOOTER-REC-1.
           05  FILLER             PIC X(20) VALUE "NUMBER OF RECORDS: ".
           05  FILLER             PIC X(18) VALUE SPACES.
           05  RECORD-NUM-O       PIC Z,ZZ9.
       01  FOOTER-REC-2.
           05  FILLER             PIC X(20) VALUE "VALID RECORDS: ".
           05  FILLER             PIC X(18) VALUE SPACES.
           05  VALID-RECORD-O     PIC Z,ZZ9.
       01  FOOTER-REC-3.
           05  FILLER             PIC X(20) VALUE "INVALID RECORDS: ".
           05  FILLER             PIC X(18) VALUE SPACES.
           05  INVALID-RECORD-O   PIC Z,ZZ9.
       01  FOOTER-REC-4.
           05  FILLER             PIC X(20) VALUE "GRAND TOTAL: ".
           05  FILLER             PIC X(5) VALUE SPACES.
           05  GRAND-TOTAL-O      PIC $$$,$$$,$$$,$$9.99.
      *
       77  INSTRUMENT-COST                      PIC S9(7)V99.
       77  SHIPPING-COST                        PIC S9(4)V99.
       77  TAX                                  PIC S9(3)V99.
       77  TOTAL                                PIC S9(8)V99.
       77  GRAND-TOTAL                          PIC S9(12)V99 VALUE 0.
       77  RECORD-NUM                           PIC 9(4) VALUE 0.
       77  VALID-RECORD                         PIC 9(4) VALUE 0.
       77  INVALID-RECORD                       PIC 9(4) VALUE 0.
      ****
       PROCEDURE DIVISION.
      ****
       MAIN.
           PERFORM 000-Housekeeping.
           PERFORM 050-Print-Header.
           PERFORM 100-Main UNTIL LASTREC = 'Y'.
           PERFORM 600-Print-Footer.
           PERFORM 700-Close-Files.
           GOBACK.
      *
       000-Housekeeping.
           INITIALIZE RFP-REC, PROP-REC.
           PERFORM 300-Open-Files.
           PERFORM 400-Read-Record.
      *
       050-Print-Header.
           WRITE RFPOUT-REC FROM HEADER-REC-0.
           WRITE RFPOUT-REC FROM HEADER-REC-1.
           WRITE RFPOUT-REC FROM HEADER-REC-2.
           WRITE RFPOUT-REC FROM HEADER-REC-3.
           WRITE RFPOUT-REC FROM HEADER-REC-4.
      *
       100-Main.
           PERFORM 250-Process-Data.
           PERFORM 200-Validate-Data.
           PERFORM 400-Read-Record.
      *
       200-Validate-Data.
           ADD 1 TO RECORD-NUM.
           IF ARTIST-ACCT-NO IS NUMERIC THEN
                MOVE 'Y' TO ACCT-NO
           END-IF
           IF (MUSICIAN-LNAME NOT = LOW-VALUES) OR
              (MUSICIAN-FNAME NOT = LOW-VALUES) THEN
                MOVE 'Y' TO NAMES
           END-IF
           IF (SHIP-TO = "IN") OR (SHIP-TO = "OUT") THEN
                MOVE 'Y' TO SHIP-COND
           END-IF
           IF MAX-MUSICIAN-BUDGET-AMOUNT >= 1000 AND
              MAX-MUSICIAN-BUDGET-AMOUNT <= 9999.99 THEN
                MOVE 'Y' TO MAX-BUDGET
           END-IF
           IF USED-FLAG OR NEW-FLAG OR PREMIUM-FLAG THEN
                MOVE 'Y' TO INST-QUAL
           END-IF
           IF KEYBOARD OR VOCALS OR GUITAR OR BASS OR DRUMS
              OR PERCUSSION THEN
                MOVE 'Y' TO INST-TYPE
           END-IF
           IF ROCK OR JAZZ OR FUSION OR FOLK OR CLASSICAL
              OR COUNTRY THEN
                MOVE 'Y' TO GENRE
           END-IF
           IF ACCT-VALID AND GENRE-VALID AND NAME-VALID AND TYPE-VALID
              AND QUAL-VALID AND BUDG-VALID AND COND-VALID THEN
                ADD TOTAL TO GRAND-TOTAL
                ADD 1 TO VALID-RECORD
                PERFORM 500-Write-Record
           ELSE
                ADD 1 TO INVALID-RECORD
                PERFORM 550-Write-Invalid
           END-IF.
           MOVE 'N' TO ACCT-NO.
           MOVE 'N' TO GENRE.
           MOVE 'N' TO NAMES.
           MOVE 'N' TO INST-TYPE.
           MOVE 'N' TO INST-QUAL.
           MOVE 'N' TO MAX-BUDGET.
           MOVE 'N' TO SHIP-COND.
      *
       250-Process-Data.
           MOVE ARTIST-ACCT-NO         TO ARTIST-ACCT-NO-O.
           MOVE MUSICIAN-LNAME         TO MUSICIAN-LNAME-O.
           MOVE MUSICIAN-FNAME         TO MUSICIAN-FNAME-O.
           MOVE ARTIST-MUSICAL-GENRE   TO ARTIST-MUSICAL-GENRE-O.
           EVALUATE TRUE
                WHEN KEYBOARD
                    MOVE 'KEYBOARD' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 3017.89 TO INSTRUMENT-COST
                WHEN VOCALS
                    MOVE 'VOCALS' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 599.05 TO INSTRUMENT-COST
                WHEN GUITAR
                    MOVE 'GUITAR' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 2648.99 TO INSTRUMENT-COST
                WHEN BASS
                    MOVE 'MASS' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 1876.00 TO INSTRUMENT-COST
                WHEN DRUMS
                    MOVE 'DRUMS' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 3087.22 TO INSTRUMENT-COST
                WHEN PERCUSSION
                    MOVE 'PERCUSSION' TO MUSICIAN-INSTRUMENT-TYPE-O
                    MOVE 799.99 TO INSTRUMENT-COST
           END-EVALUATE.
           EVALUATE TRUE
                WHEN USED-FLAG
                    MOVE 'USED' TO INSTRUMENT-QUALITY-O
                    COMPUTE INSTRUMENT-COST =
                        INSTRUMENT-COST * 0.8
                WHEN NEW-FLAG
                    MOVE 'NEW' TO INSTRUMENT-QUALITY-O
                WHEN PREMIUM-FLAG
                    MOVE 'PREMIUM' TO INSTRUMENT-QUALITY-O
                    COMPUTE INSTRUMENT-COST =
                        INSTRUMENT-COST * 1.2
           END-EVALUATE.
           MOVE SHIP-TO TO SHIP-TO-O.
           EVALUATE TRUE
                WHEN IN-COUNTRY
                    COMPUTE SHIPPING-COST = INSTRUMENT-COST * 0.1
                WHEN OUT-OF-COUNTRY
                    COMPUTE SHIPPING-COST = INSTRUMENT-COST * 0.2
           END-EVALUATE.
           COMPUTE TAX = INSTRUMENT-COST * 0.08.
           MOVE INSTRUMENT-COST TO COST-PER-INSTRUMENT-O.
           MOVE SHIPPING-COST TO SHIPPING-COST-O.
           MOVE TAX TO TAX-O.
           COMPUTE TOTAL = INSTRUMENT-COST + SHIPPING-COST + TAX.
           MOVE TOTAL TO TOTAL-O.
      *
       300-Open-Files.
           OPEN INPUT RFP-IN.
           OPEN OUTPUT RFP-OUT.
           OPEN OUTPUT IVL-OUT.
      *
       400-Read-Record.
           READ RFP-IN INTO RFP-REC
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       500-Write-Record.
           WRITE RFPOUT-REC FROM PROP-REC.
      *
       550-Write-Invalid.
           WRITE IVL-REC FROM RFP-REC.
      *
       600-Print-Footer.
           MOVE RECORD-NUM TO RECORD-NUM-O.
           MOVE VALID-RECORD TO VALID-RECORD-O.
           MOVE INVALID-RECORD TO INVALID-RECORD-O.
           MOVE GRAND-TOTAL TO GRAND-TOTAL-O.
           WRITE RFPOUT-REC FROM FOOTER-REC-0.
           WRITE RFPOUT-REC FROM FOOTER-REC-1.
           WRITE RFPOUT-REC FROM FOOTER-REC-2.
           WRITE RFPOUT-REC FROM FOOTER-REC-3.
           WRITE RFPOUT-REC FROM FOOTER-REC-4.
      *
       700-Close-Files.
           CLOSE RFP-IN, RFP-OUT, IVL-OUT.

