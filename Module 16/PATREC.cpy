       01  WS-INPUT-REC.
           05  PATIENT-NBR             PIC 9(05).
           05  PATIENT-NAME.
               10 LAST-NAME            PIC X(10).
               10 FIRST-NAME           PIC X(10).
           05  PATIENT-PHONE           PIC X(10).
           05  PATIENT-TYPE            PIC X(01).
               88 INPATIENT                      VALUE "I".
               88 OUTPATIENT                     VALUE "O".
               88 VALID-TYPE                     VALUES ARE "I", "O".
           05  BED-IDENTITY            PIC 9(04).
           05  DATE-ADMIT              PIC X(10).
           05  AMT-PER-DAY             PIC 9(05)V99.
           05  DIAGNOSTIC-CODE         PIC 9(03).
           05  INS-TYPE                PIC X(03).
           05  HOSPITAL-STAY-LTH       PIC 9(03).
           05  PATIENT-TOT-AMT         PIC 9(07)V99.
           05  PCP-ID                  PIC X(06).
           05  IN-OUT-NETWORK          PIC X(01).
               88 IN-NETWORK                     VALUE "I".
               88 OUT-OF-NETWORK                 VALUE "N".
           05  COPAY                   PIC S9(03).
           05  DEDUCTIBLE              PIC S9(04).