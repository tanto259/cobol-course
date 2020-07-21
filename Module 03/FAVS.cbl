       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
      *******************************************************************
      * This program display information about a particular music group *
      *******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FAV-REC.
           05  ARTIST-NAME              PIC X(30).
           05  NUMBER-OF-MUSICIANS      PIC 9(02).
           05  MUSICAL-GENRE            PIC X(12).
           05  COST.
                10  TOTAL-COST          PIC 9(04)V99.
                10  CD-COST             PIC 9(03)V99.
                10  SHIPPING-COST       PIC 9(02)V99.
                10  TAX                 PIC 9(02)V99.
           05  BAND-IS-STILL-TOGETHER   PIC X(01).
       PROCEDURE DIVISION.
           MOVE "Jazzy Bondz"       TO ARTIST-NAME.
           MOVE 2                   TO NUMBER-OF-MUSICIANS.
           MOVE "Jazz"              TO MUSICAL-GENRE.
           MOVE 23.50               TO CD-COST.
           MOVE 8.50                TO SHIPPING-COST.
           MOVE 0.10                TO TAX.
           MOVE "F"                 TO BAND-IS-STILL-TOGETHER.
           COMPUTE TOTAL-COST =
                CD-COST + (TAX * CD-COST) + SHIPPING-COST.
           DISPLAY "Group Name: " ARTIST-NAME.
           DISPLAY "Genre: " MUSICAL-GENRE.
           IF BAND-IS-STILL-TOGETHER = "T"
                DISPLAY "Status: Active"
           ELSE
                DISPLAY "Status: Inactive".
           DISPLAY "Number of Artist: " NUMBER-OF-MUSICIANS.
           DISPLAY "--------------------".
           DISPLAY "CD Cost: " CD-COST.
           DISPLAY "Shipping Cost: " SHIPPING-COST.
           DISPLAY "Tax Rate: " TAX.
           DISPLAY "--------------------".
           DISPLAY "Total Cost: " TOTAL-COST.
           GOBACK.
