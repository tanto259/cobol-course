       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SEARCH01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-DATA.
           05  FILLER           PIC X(40)
                   VALUE "0100001123456100198765432112345678999999".
           05  FILLER           PIC X(40)
                   VALUE "0200002123457100398765432112345678899997".
           05  FILLER           PIC X(40)
                   VALUE "0300003123458100598765432112345678099995".
           05  FILLER           PIC X(40)
                   VALUE "0400004123459100798765432112345677299993".
           05  FILLER           PIC X(40)
                   VALUE "0500005123460100998765432112345676499991".
           05  FILLER           PIC X(40)
                   VALUE "0600006123461101198765432112345675699989".
           05  FILLER           PIC X(40)
                   VALUE "0700007123462101398765432112345674899987".
           05  FILLER           PIC X(40)
                   VALUE "0800008123463101598765432112345674099985".
           05  FILLER           PIC X(40)
                   VALUE "0900009123464101798765432112345673299983".
           05  FILLER           PIC X(40)
                   VALUE "1000010123465101998765432112345672499981".

       01  TABLE-A REDEFINES TABLE-DATA.
           05  TABLE-ENTRY OCCURS 10 TIMES
                   ASCENDING KEY-1, KEY-2
                   DESCENDING KEY-3
                   INDEXED BY INDX-1.
               10  PART-1                  PIC 99.
               10  KEY-1                   PIC 9(5).
               10  PART-2                  PIC 9(6).
               10  KEY-2                   PIC 9(4).
               10  PART-3                  PIC 9(18).
               10  KEY-3                   PIC 9(5).

       77  VALUE-1                 PIC 9(5) VALUE 00009.
       77  VALUE-2                 PIC 9(4) VALUE 1017.
       77  VALUE-3                 PIC 9(5) VALUE 99983.

       77  OUTPUT-AREA             PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
           SEARCH ALL TABLE-ENTRY
           AT END
               DISPLAY "NOT FOUND"
           WHEN KEY-1 (INDX-1) = VALUE-1 AND
                KEY-2 (INDX-1) = VALUE-2 AND
                KEY-3 (INDX-1) = VALUE-3
               MOVE PART-1 (INDX-1) TO OUTPUT-AREA
           END-SEARCH.
           DISPLAY OUTPUT-AREA.
           GOBACK.