       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT2PAYROLL.
       AUTHOR.  MEL SANSCHAGRIN. CHANDLER NEWMAN-REED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  PAYROLL-REGISTER-OUT
               ASSIGN  "PAYROLLREGISTER.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  PAYROLL-SUMMARY-OUT
               ASSIGN "PAYROLLSUMMARY.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYRECORD-IN
               ASSIGN "EMPFILE2.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       
       FD PAYRECORD-IN.
       01 PAYROLL-RECORD.
           05  EMPLOYEE-NUMBER     PIC 9(9).
           05  EMPLOYEE-LAST-NAME  PIC X(13).
           05  EMPLOYEE-INITIALS   PIC X(2).
           05  HOURLY-PAY-RECORD   PIC 9(2)V99.
           05  HOURS-WORKED        PIC 9(2)V99.
           05  UNION-MEMBER        PIC X(1).
       
       FD PAYROLL-REGISTER-OUT.
       01  REPORT-HEADER-OUT PIC X(29).
       01  COLUMN-HEADERS-OUT PIC X(72).
       01  DETAIL-LINE-OUT.
           05  DL-EMPLOYEE-INITIALS-OUT    PIC X(2).
           05  FILLER                      PIC X(4)    VALUE SPACE.
           05  DL-EMPLOYEE-LAST-NAME-OUT   PIC X(13).
           05  FILLER                      PIC X(2)    VALUE SPACE.
           05  GROSS-PAY-OUT               PIC Z,ZZZ.99.
           05  FILLER                      PIC X(4)    VALUE SPACE.
           05  TAX-DEDUCTION-OUT           PIC ZZZ.99.
           05  FILLER                      PIC X(4)    VALUE SPACE.
           05  HEALTH-DEDUCTION-OUT        PIC ZZZ.99.
           05  FILLER                      PIC X(4)    VALUE SPACE.
           05  UNION-DUES-OUT              PIC Z9.99.
           05  FILLER                      PIC X(4)    VALUE SPACE.
           05  NET-PAY-OUT                 PIC ZZZ,ZZZ.99.
               
       FD PAYROLL-SUMMARY-OUT.
       01 TOTAL-GROSS-OUT.
          05 FILLER                     PIC X(3).
          05 TOTAL-GROSS-HEADER-OUT     PIC X(11).
          05 FILLER                     PIC X(15).
          05 TOTAL-GROSS-PAY-OUT        PIC $ZZ,ZZZ.99.
       01 TOTAL-NET-OUT.
          05 FILLER                     PIC X(3).
          05 TOTAL-NET-HEADER-OUT       PIC X(9).
          05 FILLER                     PIC X(17).
          05 TOTAL-NET-PAY-OUT          PIC $ZZ,ZZZ.99.
       01 TOTAL-PAY-REC-OUT.
          05 FILLER                     PIC X(3).
          05 PAY-REC-HEADER-OUT         PIC X(17).
          05 FILLER                     PIC X(9).
          05 TOTAL-PAY-RECORDS-OUT      PIC 9(2).
          05 FILLER                     PIC X(8).
       01 TOTAL-PAY-REG-OUT.
          05 FILLER                     PIC X(3).
          05 REG-REC-HEADER-OUT         PIC X(23).
          05 FILLER                     PIC X(3).
          05 TOTAL-REGISTER-RECORDS-OUT PIC 9(2).
          05 FILLER                     PIC X(8).
       
       WORKING-STORAGE SECTION.
       01  WS-REPORT-HEADER.
           05  FILLER  PIC X(6) VALUE SPACE.
           05  HEADER  PIC X(23) VALUE 'PAYROLL REGISTER REPORT'.
           
       01  WS-COLUMN-HEADERS.
           05  EMPLOYEE-NAME   PIC X(21)   VALUE "EMPLOYEE NAME".
           05  GROSS           PIC X(12)   VALUE "GROSS".
           05  TAX             PIC X(11)   VALUE "TAX".
           05  HEALTH          PIC X(9)    VALUE "HEALTH".
           05  UNION           PIC X(16)    VALUE "UNION DUES".
           05  NET             PIC X(9)    VALUE "NET".

       01  FLAGS-AND-COUNTERS.
           05  EOF-FLAG        PIC X(1)  VALUE "N".
           
       01  WS-PAYROLL-SUMMARY.
           05 WS-TOTAL-GROSS.
              10 FILLER                 PIC X(3)      VALUE SPACE.
              10 TOTAL-GROSS-HEADER     PIC X(11)     VALUE 
                   "TOTAL GROSS".
              10 FILLER                 PIC X(15).
              10 TOTAL-GROSS-PAY        PIC 9(7)V99   VALUE ZERO.
           05 WS-TOTAL-NET.
               10 FILLER                 PIC X(3)      VALUE SPACE.
               10 TOTAL-NET-HEADER       PIC X(9)      VALUE 
                   "TOTAL NET".
               10 FILLER                 PIC X(17).
               10 TOTAL-NET-PAY          PIC 9(7)V99   VALUE ZERO.
           05 WS-TOTAL-PAY-REC.
               10 FILLER                 PIC X(3)      VALUE SPACE.
               10 PAY-REC-HEADER         PIC X(17)     VALUE 
                   "TOTAL PAY RECORDS".
               10 FILLER                 PIC X(9).
               10 TOTAL-PAY-RECORDS      PIC 9(2)      VALUE ZERO.
               10 FILLER                 PIC X(8).
           05 WS-TOTAL-REG-REC.
               10 FILLER                 PIC X(3)      VALUE SPACE.
               10 REG-REC-HEADER         PIC X(23)     VALUE 
                   "TOTAL REGISTER RECORDS".
               10 FILLER                 PIC X(3).
               10 TOTAL-REGISTER-RECORDS PIC 9(2) VALUE ZERO.
               10 FILLER                 PIC X(8).
           
           
       01  DETAIL-LINE.
           05  DL-EMPLOYEE-INITIALS   PIC X(2).
           05  FILLER                 PIC X(4)    VALUE SPACE.
           05  DL-EMPLOYEE-LAST-NAME  PIC X(13).
           05  FILLER                 PIC X(2)    VALUE SPACE.
           05  GROSS-PAY              PIC 9(6)V99.
           05  FILLER                 PIC X(4)    VALUE SPACE.
           05  TAX-DEDUCTION          PIC 9(4)V99.
           05  FILLER                 PIC X(4)    VALUE SPACE.
           05  HEALTH-DEDUCTION       PIC 9(4)V99.
           05  FILLER                 PIC X(4)    VALUE SPACE.
           05  UNION-DUES             PIC 9(3)V99.
           05  FILLER                 PIC X(4)    VALUE SPACE.
           05  NET-PAY                PIC 9(7)V99.

       PROCEDURE DIVISION.
       100-CREATE-PAYROLL-FILES.
           PERFORM 200-INITIATE-CREATE-PAYROLL-FILES.
           PERFORM 200-INITIATE-CREATE-PAYREGISTER-RECORD.
           PERFORM 200-CREATE-PAYREGISTER-RECORD UNTIL EOF-FLAG = 'Y'.
           PERFORM 200-CREATE-PAYSUMMARY-FILE.
           PERFORM 200-TERMINATE-CREATE-PAYROLL-FILES.
           STOP RUN.

       200-INITIATE-CREATE-PAYROLL-FILES.
           PERFORM 700-OPEN-PAYRECORD-FILE.
           PERFORM 700-OPEN-PAYROLL-REGISTER-FILE.
           PERFORM 700-OPEN-PAYROLL-SUMMARY-FILE.
           PERFORM 700-READ-PAYRECORD-FILE-RECORD.
           
       200-INITIATE-CREATE-PAYREGISTER-RECORD.
           PERFORM 700-WRITE-PAYREGISTER-HEADERS.

       200-CREATE-PAYREGISTER-RECORD.
           PERFORM 700-PROCESS-PAYREGISTER.
           PERFORM 700-OUTPUT-PAYREGISTER.
           PERFORM 700-READ-PAYRECORD-FILE-RECORD.
           
       200-TERMINATE-CREATE-PAYROLL-FILES.
           PERFORM   700-CLOSE-FILES.
           
       200-CREATE-PAYSUMMARY-FILE.
           PERFORM 700-WRITE-PAYSUMMARY-RECORDS.
           
       700-WRITE-PAYREGISTER-HEADERS.
           WRITE REPORT-HEADER-OUT FROM WS-REPORT-HEADER.
           WRITE COLUMN-HEADERS-OUT FROM WS-COLUMN-HEADERS.
      
       700-PROCESS-PAYREGISTER.
           PERFORM 1000-COMPUTE-GROSS-PAY.
           PERFORM 1000-COMPUTE-TOTAL-GROSS.
           PERFORM 1000-COMPUTE-TAX-DEDUCTION.
           PERFORM 1000-COMPUTE-HEALTH-DEDUCTION.
           PERFORM 1000-COMPUTE-UNION-DUES.
           PERFORM 1000-COMPUTE-NET-PAY.
           PERFORM 1000-COMPUTE-TOTAL-NET.

       700-OUTPUT-PAYREGISTER.
           PERFORM 1000-MOVE-PAYREGISTER-RECORDS.
           PERFORM 1000-WRITE-PAYREGISTER-FILE.
           
       700-READ-PAYRECORD-FILE-RECORD.
           READ PAYRECORD-IN INTO PAYROLL-RECORD
               AT END 
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END 
                   ADD 1 TO TOTAL-PAY-RECORDS.
       
       700-DISPLAY-PAYRECORD-RECORD.
           DISPLAY PAYROLL-RECORD.

       700-OPEN-PAYRECORD-FILE.
           OPEN INPUT PAYRECORD-IN.
           
       700-OPEN-PAYROLL-REGISTER-FILE.
           OPEN OUTPUT PAYROLL-REGISTER-OUT.
           
       700-OPEN-PAYROLL-SUMMARY-FILE.
           OPEN OUTPUT PAYROLL-SUMMARY-OUT.

       700-CLOSE-FILES.
           CLOSE PAYRECORD-IN.
           CLOSE PAYROLL-REGISTER-OUT.
           CLOSE PAYROLL-SUMMARY-OUT.
           
       700-WRITE-PAYSUMMARY-RECORDS.
           MOVE WS-TOTAL-GROSS TO TOTAL-GROSS-OUT.
           MOVE TOTAL-GROSS-PAY TO TOTAL-GROSS-PAY-OUT.
           WRITE TOTAL-GROSS-OUT.
           MOVE WS-TOTAL-NET TO TOTAL-NET-OUT.
           MOVE TOTAL-NET-PAY TO TOTAL-NET-PAY-OUT.
           WRITE TOTAL-NET-OUT.
           MOVE WS-TOTAL-PAY-REC TO TOTAL-PAY-REC-OUT.
           MOVE TOTAL-PAY-RECORDS TO TOTAL-PAY-RECORDS-OUT.
           WRITE TOTAL-PAY-REC-OUT.
           MOVE WS-TOTAL-REG-REC TO TOTAL-PAY-REG-OUT.
           MOVE TOTAL-REGISTER-RECORDS TO TOTAL-REGISTER-RECORDS-OUT.
           WRITE TOTAL-PAY-REG-OUT.
           
           
       1000-COMPUTE-GROSS-PAY.
           IF HOURS-WORKED GREATER THAN 40
               COMPUTE GROSS-PAY = (HOURLY-PAY-RECORD * HOURS-WORKED) + 
               (HOURLY-PAY-RECORD * (HOURS-WORKED - 40) * 1.5)
           ELSE
               COMPUTE GROSS-PAY = HOURLY-PAY-RECORD * HOURS-WORKED.
       
       1000-COMPUTE-TAX-DEDUCTION.
           COMPUTE TAX-DEDUCTION = 0.20 * GROSS-PAY.
           
       1000-COMPUTE-HEALTH-DEDUCTION.
           COMPUTE HEALTH-DEDUCTION = 0.02 * GROSS-PAY.
           
       1000-COMPUTE-TOTAL-GROSS.
           ADD GROSS-PAY TO TOTAL-GROSS-PAY.
       
       1000-COMPUTE-TOTAL-NET.
           ADD NET-PAY TO TOTAL-NET-PAY.
           
       1000-COMPUTE-UNION-DUES.
           IF UNION-MEMBER = 'U'
               MOVE 20.00 TO UNION-DUES
           ELSE
               MOVE 0 TO UNION-DUES.
       
       1000-COMPUTE-NET-PAY.
           COMPUTE NET-PAY = GROSS-PAY - TAX-DEDUCTION - 
           HEALTH-DEDUCTION - UNION-DUES.
           
       1000-MOVE-PAYREGISTER-RECORDS.
           MOVE DETAIL-LINE TO DETAIL-LINE-OUT.
           MOVE EMPLOYEE-INITIALS TO DL-EMPLOYEE-INITIALS-OUT.
           MOVE EMPLOYEE-LAST-NAME TO DL-EMPLOYEE-LAST-NAME-OUT.
           MOVE GROSS-PAY TO GROSS-PAY-OUT.
           MOVE TAX-DEDUCTION TO TAX-DEDUCTION-OUT.
           MOVE HEALTH-DEDUCTION TO HEALTH-DEDUCTION-OUT.
           MOVE UNION-DUES TO UNION-DUES-OUT.
           MOVE NET-PAY TO NET-PAY-OUT.
           
       1000-WRITE-PAYREGISTER-FILE.
           WRITE DETAIL-LINE-OUT.
           ADD 1 TO TOTAL-REGISTER-RECORDS.