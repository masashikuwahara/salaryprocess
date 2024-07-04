      ******************************************************************
      * Author:kuwahara
      * Date:July.1.2024
      * Purpose:Lerning
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALCULATION.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT EMPLOYEE-F ASSIGN TO 'employee.txt'
                       ORGANIZATION IS LINE SEQUENTIAL.
                   SELECT PAYMENT-F ASSIGN TO "payment.txt"
                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-F.
       01 EMPLOYEE.
           03 EMP-ID PIC 9(6).
           03 EMP-STATUS PIC 9(1).
           03 DATE-OF-EMP.
               05 EMP-YEAR PIC 9(4).
               05 EMP-MM PIC 9(2).
               05 EMP-DD PIC 9(2).
           03 BASIC-SALARY PIC 9(6).
       FD PAYMENT-F.
       01 PAYMENT.
           03 PAYMENT-ID PIC 9(6).
           03 PAYMENT-AMOUNT3 PIC 9(6).
           03 MONEY.
               05 MONEY-10000 PIC 9(2).
               05 MONEY-5000 PIC 9(2).
               05 MONEY-1000 PIC 9(2).
               05 MONEY-500 PIC 9(2).
               05 MONEY-100 PIC 9(2).
       WORKING-STORAGE SECTION.
       01 EOF PIC X VALUE "N".
       01 PAYMENT-AMOUNT PIC 9(6).
       01 PAYMENT-AMOUNT2 PIC 9999PP.
       01 RATE PIC 9V99.
       01 ANNI-DAY PIC 9(8) VALUE 20130000.
       01 LEN PIC S9(4).
       01 LEN2 PIC S9(4).
       01 BASIC-RESULT PIC 9(6).
       01 BASIC-RESULT2 PIC 9(6).
       01 BASIC-RESULT3 PIC 9(6).
       01 BASIC-RESULT4 PIC 9(6).
       01 BASIC-RESULT5 PIC 9(6).
       PROCEDURE DIVISION.
      *----------<メインルーチン>----------
       MAIN SECTION.
           OPEN INPUT EMPLOYEE-F
           OUTPUT PAYMENT-F.
           PERFORM PROCESSING UNTIL EOF="Y"
           CLOSE EMPLOYEE-F
           PAYMENT-F.
           STOP RUN.
      *----------<サブルーチン>----------
       PROCESSING SECTION.
               READ EMPLOYEE-F
                   AT END MOVE "Y" TO EOF.
                   IF EOF="N"
                       PERFORM PROCESSING-EMPLOYEE
                   END-IF.
      *PROCESSING SECTION出口
       EXIT.
       PROCESSING-EMPLOYEE SECTION.
      *従業員番号を支給ファイルのレコード様式にMOVEする
               MOVE EMP-ID TO PAYMENT-ID.
               IF EMP-ID = 000000 OR PAYMENT-ID = 000000 THEN
                   DISPLAY "EMP-NUMBER INPUT ERROR"
               END-IF.
      *記念日から入社日を引き勤続年数を算出
               IF EMP-YEAR>0 THEN
                COMPUTE LEN = 2013 * 12 + 04 - EMP-YEAR * 12 + EMP-MM
               ELSE
                MOVE -100 TO LEN
               END-IF.
               DISPLAY "EMP-YEAR:"EMP-YEAR.
               DISPLAY "EMP-MM:"EMP-MM.
               DISPLAY "Length of service:"LEN.
               COMPUTE LEN2 = LEN / 12
               DISPLAY "LEN2:"LEN2

               IF LEN2<4 AND EMP-STATUS = 0
               THEN
                  MOVE 0.05 TO RATE
                  DISPLAY "Less than 3 years, General staff"
               ELSE
                IF LEN2<4 AND EMP-STATUS = 1
                THEN
                  MOVE 0.10 TO RATE
                  DISPLAY "Less than 3 years, Manager"
                ELSE
                 IF (LEN2>4) AND (LEN2<10) AND (EMP-STATUS = 0)
                 THEN
                  MOVE 0.10 TO RATE
                  DISPLAY "Over 3 years, upto 9 years, General staff"
                 ELSE
                  IF (LEN2>4) AND (LEN2<10) AND (EMP-STATUS = 1)
                  THEN
                   MOVE 0.15 TO RATE
                   DISPLAY "Over 3 years, upto 9 years, Manager"
                  ELSE
                   IF (LEN2>10) AND (LEN2<21) AND (EMP-STATUS = 0)
                   THEN
                    MOVE 0.15 TO RATE
                    DISPLAY "Over 10 years, General staff"
                    DISPLAY RATE
                   ELSE
                    IF (LEN2>10) AND (LEN2<21)AND (EMP-STATUS = 1)
                    THEN
                     MOVE 0.20 TO RATE
                     DISPLAY "Over 10 years, Manager"
                     DISPLAY RATE
                    ELSE
                     DISPLAY "ERROR DATA"
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF.
      *基本給が000000の場合エラーを出力する
               IF BASIC-SALARY = 000000
                   DISPLAY "BASIC-SALARY INPUT ERROR"
               END-IF.
      *入社日が2013年より先の場合はRATEに0を格納する
               IF LEN2<0 THEN
                   MOVE 0 TO RATE
                   END-IF.
               
      *格納したRATEに基本給を掛け支給金額を算出
               COMPUTE PAYMENT-AMOUNT = BASIC-SALARY * RATE.
               MOVE PAYMENT-AMOUNT TO PAYMENT-AMOUNT2
               MOVE PAYMENT-AMOUNT2 TO PAYMENT-AMOUNT3
      *金種の数を算出
               DIVIDE 10000 INTO PAYMENT-AMOUNT2 GIVING MONEY-10000
               REMAINDER BASIC-RESULT2.
               DIVIDE 5000 INTO BASIC-RESULT2 GIVING MONEY-5000
               REMAINDER BASIC-RESULT3.
               DIVIDE 1000 INTO BASIC-RESULT3 GIVING MONEY-1000
               REMAINDER BASIC-RESULT4.
               DIVIDE 500 INTO BASIC-RESULT4 GIVING MONEY-500
               REMAINDER BASIC-RESULT5.
               DIVIDE 100 INTO BASIC-RESULT5 GIVING MONEY-100.
      *ファイルに書き出す
               WRITE PAYMENT.
               
               DISPLAY "-----From here console check-----"
               DISPLAY "EMPLOYEE-ID:"EMP-ID.
               DISPLAY "EMPLOYEE-STATUS:"EMP-STATUS.
               DISPLAY "DATE-OF-EMP:"DATE-OF-EMP.
               DISPLAY "BASIC-SALARY:"BASIC-SALARY.
               DISPLAY "Length of service:"LEN2.
               DISPLAY "RATE:"RATE.
               DISPLAY "PAYMENT-ID:"PAYMENT-ID.
               DISPLAY "PAYMENT AMOUNT:"PAYMENT-AMOUNT3.
               DISPLAY "10000:"MONEY-10000.
               DISPLAY "5000:"MONEY-5000.
               DISPLAY "1000:"MONEY-1000.
               DISPLAY "500:"MONEY-500.
               DISPLAY "100:"MONEY-100.
      *作業領域を初期化する
                INITIALIZE PAYMENT-AMOUNT 
                PAYMENT-AMOUNT2 RATE LEN BASIC-RESULT BASIC-RESULT2
                BASIC-RESULT3 BASIC-RESULT4 BASIC-RESULT5.
           
       END PROGRAM SALARY-CALCULATION.
