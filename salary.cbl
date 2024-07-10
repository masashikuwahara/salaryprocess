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
                   SELECT PAYMENT-F ASSIGN TO 'payment.txt'
                       ORGANIZATION IS LINE SEQUENTIAL.
                   SELECT ERROR-F ASSIGN TO 'error.txt'
                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-F.
       01 EMPLOYEE.
           03 EMP-ID PIC 9(6).
           03 EMP-STATUS PIC 9(1).
           03 DATE-OF-EMP.
               05 EMP-YY PIC 9(4).
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
       FD ERROR-F.
       01 ERRORS.
           03 NUM-ERR PIC X(30).
           03 SAL-ERR PIC X(30).
           03 STA-ERR PIC X(30).
           03 DAT-ERR PIC X(30).
           03 ID-ERR PIC X(30).
           03 DUP-ERR PIC X(30).
       WORKING-STORAGE SECTION.
       01 EOF PIC X VALUE "N".
       01 EMP-ID-COMP PIC 9(6) VALUE 000000.
       01 PAYMENT-AMOUNT PIC 9(6).
       01 PAYMENT-AMOUNT2 PIC 9999PP.
       01 RATE PIC 9V99.
       01 ANNI-DAY PIC 9(8) VALUE 20130000.
       01 LEN PIC S9(4).
       01 LEN2 PIC 9(4).
       01 BASIC-RESULT PIC 9(6).
       01 BASIC-RESULT2 PIC 9(6).
       01 BASIC-RESULT3 PIC 9(6).
       01 BASIC-RESULT4 PIC 9(6).
       01 BASIC-RESULT5 PIC 9(6).
       PROCEDURE DIVISION.
      *----------<メインルーチン>----------
       MAIN SECTION.
           OPEN INPUT EMPLOYEE-F
           OUTPUT PAYMENT-F ERROR-F
           PERFORM PROCESSING UNTIL EOF="Y"
           CLOSE EMPLOYEE-F
           PAYMENT-F
           ERROR-F.
           STOP RUN.
      *----------<サブルーチン>----------
       PROCESSING SECTION.
           READ EMPLOYEE-F
           AT END MOVE "Y" TO EOF.
           DISPLAY "-----From here----- ".
           DISPLAY "EMPLOYEE NUMBER:"EMP-ID.
           IF EOF="N"
               PERFORM CHECK-RECORD
               DISPLAY "PASS0001"
           ELSE
               DISPLAY "PASS0002"
           END-IF.
      *PROCESSING SECTION出口
       EXIT.

      *>-----ここから入力データの判定-----

       CHECK-RECORD SECTION.
      *>  入力ファイルのデータ項目が数字であるかどうかを判定する
           IF EMPLOYEE IS NUMERIC THEN
               PERFORM CHECK-RECORD2
               DISPLAY "PASS0003"
           ELSE
               MOVE "NUMERIC INPUT ERROR!" TO NUM-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0004"
           END-IF.
       EXIT.

      *>  入力ファイルの基本給が000000以上であるかどうかを判定する
       CHECK-RECORD2 SECTION.
           IF BASIC-SALARY>000000 THEN
               PERFORM CHECK-RECORD3
               DISPLAY "PASS0005"
           ELSE
               MOVE "BASIC-SALARY INPUT ERROR!" TO SAL-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0006"
           END-IF.
       EXIT.

      *>  入力ファイルの従業員ステータスが0,1以外かどうかを判定する
       CHECK-RECORD3 SECTION.
           IF EMP-STATUS<=2 THEN
               PERFORM CHECK-RECORD4
               DISPLAY "PASS0007"
           ELSE
               MOVE "EMP-STATUS INPUT ERROR!" TO STA-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0008"
       EXIT.

      *>  入力ファイルの入社日が正しいかを判定する
       CHECK-RECORD4 SECTION.
           IF (EMP-YY>=1993)AND(EMP-YY<=2014) THEN
               PERFORM CHECK-RECORD5
               DISPLAY "PASS0009"
           ELSE
               MOVE "DATE INPUT ERROR!" TO DAT-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0010"
           END-IF.
       EXIT.

      *>  入力ファイルの従業員番号が000001以上かを判定する
       CHECK-RECORD5 SECTION.
           IF EMP-ID>000000 THEN
               PERFORM CHECK-RECORD6
               DISPLAY "PASS0011"
           ELSE
               MOVE "EMP-ID INPUT ERROR!" TO ID-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0012"
           END-IF.
       EXIT.

       CHECK-RECORD6 SECTION.
      *>  入力ファイルの従業員番号が重複していないかを判定する
           IF EMP-ID IS NOT = EMP-ID-COMP THEN
               PERFORM PROCESSING-EMPLOYEE
               DISPLAY "PASS0013"
           ELSE
               MOVE "EMP-ID DUPLICATE ERROR!" TO DUP-ERR
               WRITE ERRORS
               INITIALIZE ERRORS
               DISPLAY "PASS0014"
           END-IF.
       EXIT.

       PROCESSING-EMPLOYEE SECTION.
      *従業員番号を支給ファイルのレコード様式にMOVEする
               MOVE EMP-ID TO PAYMENT-ID.
      *記念日から入社日を引き勤続期間を算出
                COMPUTE LEN = 2013 * 12 + 4 - EMP-YY * 12 - EMP-MM
      *>  算出した勤続期間から支給係数を算出する
               IF (LEN>0) AND (LEN<=36) AND (EMP-STATUS = 0)
               THEN
                  MOVE 0.05 TO RATE
                  DISPLAY "PASS0015"
               ELSE
                IF (LEN>0) AND (LEN<=36) AND (EMP-STATUS) = 1
                THEN
                  MOVE 0.10 TO RATE
                  DISPLAY "PASS0016"
                ELSE
                 IF (LEN>36) AND (LEN<=120) AND (EMP-STATUS = 0)
                 THEN
                  MOVE 0.10 TO RATE
                  DISPLAY "PASS0017"
                 ELSE
                  IF (LEN>36) AND (LEN<=120) AND (EMP-STATUS = 1)
                  THEN
                   MOVE 0.15 TO RATE
                   DISPLAY "PASS0018"
                  ELSE
                   IF (LEN>120) AND (LEN<=240) AND (EMP-STATUS = 0)
                   THEN
                    MOVE 0.15 TO RATE
                    DISPLAY "PASS0019"
                   ELSE
                    IF (LEN>10) AND (LEN<=240)AND (EMP-STATUS = 1)
                    THEN
                     MOVE 0.20 TO RATE
                     DISPLAY "PASS0020"
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
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

      *>       次の従業員番号重複の判定のためにワークEMP-ID-COMPにEMP-IDを格納
               MOVE EMP-ID TO EMP-ID-COMP.
      *作業領域を初期化する
                INITIALIZE PAYMENT-AMOUNT PAYMENT-AMOUNT2 RATE LEN
                BASIC-RESULT BASIC-RESULT2
                BASIC-RESULT3 BASIC-RESULT4 BASIC-RESULT5.

       END PROGRAM SALARY-CALCULATION.
