      ******************************************************************
      * Author:kuwahara
      * Date:July.1.2024
      * Purpose:Learning
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALCULATION.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT EMPLOYEE-F ASSIGN TO 'in1.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS F-STATUS.
                   SELECT PII-F ASSIGN TO 'in2.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS F-STATUS2.
                   SELECT PAYMENT-F ASSIGN TO 'payment.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS F-STATUS3.
                   SELECT ERROR-F ASSIGN TO 'error1.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS F-STATUS4.
                   SELECT ERROR-F2 ASSIGN TO 'error2.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       FILE STATUS IS F-STATUS5.
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
       FD PII-F.
       01 PII.
           03 PII-ID PIC 9(6).
           03 PII-NAME PIC N(20).
       FD PAYMENT-F.
       01 PAYMENT.
           03 PAYMENT-ID PIC 9(6).
           03 PAYMENT-NAME PIC N(20).
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
       FD ERROR-F2.
       01 ERRORS2.
           03 PII-ID-ERR2 PIC X(30).
           03 PII-DUP-ERR2 PIC X(30).
           03 PII-INPUT-ERR2 PIC X(30).
       WORKING-STORAGE SECTION.
       01 F-STATUS PIC XX.
       01 F-STATUS2 PIC XX.
       01 F-STATUS3 PIC XX.
       01 F-STATUS4 PIC XX.
       01 F-STATUS5 PIC XX.
       01 EOF PIC X VALUE "N".
       01 WORK-ERROR PIC X(270).
       01 ERRORFLAG-IN1 PIC X(3).
       01 ERRORFLAG-IN2 PIC X(3).
       01 EMP-ID-COMP PIC 9(6) VALUE 111111.
       01 PII-ID-COMP PIC 9(6) VALUE 111111.
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
           OPEN INPUT EMPLOYEE-F.
           OPEN INPUT PII-F.
           OPEN OUTPUT PAYMENT-F.
           OPEN OUTPUT ERROR-F.
           OPEN OUTPUT ERROR-F2.
           INITIALIZE WORK-ERROR.
           IF F-STATUS NOT = "00"
               DISPLAY "An error occurred while opening the file."
           ELSE
               DISPLAY "File opened successfully."
           END-IF.

           IF F-STATUS2 NOT = "00"
               DISPLAY "An error occurred while opening the file."
           ELSE
               DISPLAY "File opened successfully."
           END-IF.

           IF F-STATUS3 NOT = "00"
               DISPLAY "An error occurred while opening the file."
           ELSE
               DISPLAY "File opened successfully."
           END-IF.

           IF F-STATUS4 NOT = "00"
               DISPLAY "An error occurred while opening the file."
           ELSE
               DISPLAY "File opened successfully."
           END-IF.

           IF F-STATUS5 NOT = "00"
               DISPLAY "An error occurred while opening the file."
           ELSE
               DISPLAY "File opened successfully."
           END-IF.

           PERFORM PROCESSING.
      *>      CLOSE EMPLOYEE-F
      *>      PII-F
      *>      PAYMENT-F
      *>      ERROR-F
      *>      ERROR-F2.
           STOP RUN.
      *----------<サブルーチン>----------
      *>  編集処理
       PROCESSING SECTION.
      *>  従業員ファイル読み込み
           IF EOF="Y" THEN
               DISPLAY "PASS0001"
           ELSE
               DISPLAY "PASS0002"
               PERFORM EMPLOYEE-FILE
           END-IF.
      *>  個人データファイル読み込み
           IF EOF="Y" THEN
              DISPLAY "PASS0003"
           ELSE
               DISPLAY "PASS0004"
               PERFORM PII-FILE
           END-IF.
      *>  マッチングをループさせる
               PERFORM MATCHING UNTIL EOF="Y".
       EXIT.

      *>----------マッチング----------
      *>  ここでループ処理
       MATCHING SECTION.
           IF ERRORS = WORK-ERROR THEN
               DISPLAY "PASS0005"
               IF EMP-ID = PII-ID THEN
                   DISPLAY "PASS0006"
                   PERFORM PROCESSING-EMPLOYEE
                   PERFORM EMPLOYEE-FILE
                   PERFORM PII-FILE
               ELSE
                   IF EMP-ID < PII-ID THEN
                       DISPLAY "PASS0007"
                       PERFORM EMPLOYEE-FILE
                   ELSE
                       PERFORM PII-FILE
                   END-IF
               END-IF
           ELSE
               DISPLAY "PASS0008"
               INITIALIZE ERRORS
      *>          PERFORM PROCESSING
           END-IF.
       EXIT.

       EMPLOYEE-FILE SECTION.
           READ EMPLOYEE-F
           AT END MOVE "Y" TO EOF.
           INITIALIZE ERRORFLAG-IN1
           INITIALIZE ERRORS
           DISPLAY "ERRORCHECK:"ERRORFLAG-IN1
           DISPLAY "EMPLOYEE-NUMBER:"EMP-ID.
           IF EOF="Y" THEN
               DISPLAY "PASS0009"
           ELSE
               DISPLAY "PASS0010"
               PERFORM IN1-CHECK
           END-IF.
       EXIT.

       PII-FILE SECTION.
           READ PII-F
           AT END MOVE "Y" TO EOF.
           DISPLAY "PII-NUMBER:"PII-ID.
           DISPLAY "PII-NAME:"PII-NAME.
           INITIALIZE ERRORFLAG-IN2
           INITIALIZE ERRORS2
           DISPLAY "ERRORCHECK2:"ERRORFLAG-IN2
           IF EOF="Y"
               DISPLAY "PASS0011"
               PERFORM CLOSE-F
           ELSE
               DISPLAY "PASS0012"
               PERFORM IN2-CHECK
           END-IF.
       EXIT.

       CLOSE-F SECTION.
           CLOSE EMPLOYEE-F.
           CLOSE PII-F.
           CLOSE PAYMENT-F.
           CLOSE ERROR-F.
           CLOSE ERROR-F2.
       EXIT.

      *>----------ここから従業員ファイルのエラー判定----------
       IN1-CHECK SECTION.
      *>  入力ファイルのデータ項目がNUMERICであるかどうかを判定する
           IF EMPLOYEE IS NUMERIC THEN
               DISPLAY "PASS0013"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0014"
               INITIALIZE ERRORFLAG-IN1
               MOVE "NUMERIC INPUT ERROR!" TO NUM-ERR
               WRITE ERRORS
           END-IF.

           IF NUM-ERR = " " THEN
               DISPLAY "PASS0015"
               PERFORM CHECK-RECORD2
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0016"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>  入力ファイルの基本給が000001以上であるかどうかを判定する
       CHECK-RECORD2 SECTION.
           IF BASIC-SALARY>000000 THEN
               DISPLAY "PASS0017"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0018"
               INITIALIZE ERRORFLAG-IN1
               MOVE "BASIC-SALARY INPUT ERROR!" TO SAL-ERR
               WRITE ERRORS
           END-IF.

           IF SAL-ERR = " " THEN
               DISPLAY "PASS0019"
               PERFORM CHECK-RECORD3
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0020"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>  入力ファイルの従業員ステータスが0,1以外かどうかを判定する
       CHECK-RECORD3 SECTION.
           IF EMP-STATUS<2 THEN
               DISPLAY "PASS0021"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0022"
               INITIALIZE ERRORFLAG-IN1
               MOVE "EMP-STATUS INPUT ERROR!" TO STA-ERR
               WRITE ERRORS
           END-IF.

           IF STA-ERR = " " THEN
               DISPLAY "PASS0023"
               PERFORM CHECK-RECORD4
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0024"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>  入力ファイルの入社日が正しいかを判定する
       CHECK-RECORD4 SECTION.
           IF (EMP-YY>=1993)AND(EMP-YY<=2014) THEN
               DISPLAY "PASS0025"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0026"
               INITIALIZE ERRORFLAG-IN1
               MOVE "DATE INPUT ERROR!" TO DAT-ERR
               WRITE ERRORS
           END-IF.

           IF DAT-ERR = " " THEN
               DISPLAY "PASS0027"
               PERFORM CHECK-RECORD5
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0028"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>      入力ファイルの従業員番号が重複していないか判定
       CHECK-RECORD5 SECTION.
           IF EMP-ID IS NOT = EMP-ID-COMP THEN
               DISPLAY "PASS0029"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0030"
               INITIALIZE ERRORFLAG-IN1
               MOVE "EMP-ID DUPLICATION ERROR!" TO DUP-ERR
               WRITE ERRORS
           END-IF.

           IF DUP-ERR = " " THEN
               DISPLAY "PASS0031"
               PERFORM CHECK-RECORD6
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0032"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>      EMPLOYEE-Fの従業員番号が000001以上かどうかの判定
       CHECK-RECORD6 SECTION.
           IF EMP-ID>000000 THEN
               DISPLAY "PASS0033"
               MOVE "OFF" TO ERRORFLAG-IN1
           ELSE
               DISPLAY "PASS0034"
               INITIALIZE ERRORFLAG-IN1
               MOVE "EMP-ID INPUT ERROR!" TO ID-ERR
               WRITE ERRORS
           END-IF.

           IF ERRORFLAG-IN1 = "OFF" THEN
               DISPLAY "PASS0035"
               PERFORM PII-FILE
      *>          INITIALIZE ERRORS
           ELSE
               DISPLAY "PASS0036"
               PERFORM EMPLOYEE-FILE
           END-IF.
       EXIT.

      *>----------ここから個人データファイルのエラー判定----------

      *>  PIIファイルの従業員番号がNUMERICであるかどうかの判定
       IN2-CHECK SECTION.
           IF PII-ID IS NUMERIC THEN
               DISPLAY "PASS0037"
               MOVE "OFF" TO ERRORFLAG-IN2
           ELSE
               DISPLAY "PASS0038"
               INITIALIZE ERRORFLAG-IN2
               MOVE "PII-ID NUMERIC ERROR!" TO PII-ID-ERR2
               WRITE ERRORS2
           END-IF.

           IF PII-ID-ERR2 = " " THEN
               DISPLAY "PASS0039"
               PERFORM CHECK-RECORD8
      *>          INITIALIZE ERRORS
      *>          PERFORM PROCESSING
           ELSE
               DISPLAY "PASS0040"
               PERFORM PII-FILE
           END-IF.
       EXIT.

      *>  PIIファイルの従業員番号重複の判定
       CHECK-RECORD8 SECTION.
           IF PII-ID IS NOT = PII-ID-COMP THEN
               DISPLAY "PASS0041"
               MOVE "OFF" TO ERRORFLAG-IN2
           ELSE
               DISPLAY "PASS0042"
               INITIALIZE ERRORFLAG-IN2
               MOVE "PII-ID DUPLICATION ERROR!" TO PII-DUP-ERR2
               WRITE ERRORS2
           END-IF.

           IF PII-DUP-ERR2 = " " THEN
               DISPLAY "PASS0043"
               PERFORM CHECK-RECORD9
      *>          INITIALIZE ERRORS
      *>          PERFORM PROCESSING
           ELSE
               DISPLAY "PASS0044"
               PERFORM PII-FILE
           END-IF.
       EXIT.

      *>  PIIファイルの従業員番号が000001以上かどうかの判定
       CHECK-RECORD9 SECTION.
           IF PII-ID>000000 THEN
               DISPLAY "PASS0045"
               MOVE "OFF" TO ERRORFLAG-IN2
           ELSE
               DISPLAY "PASS0046"
               INITIALIZE ERRORFLAG-IN2
               MOVE "PII-ID INPUT ERROR!" TO PII-INPUT-ERR2
               WRITE ERRORS2
           END-IF.
           DISPLAY "ERRORCHECK2:"ERRORFLAG-IN2
           IF ERRORFLAG-IN2 = "OFF" THEN
               DISPLAY "PASS0047"
               DISPLAY "ERRORCHECK2:"ERRORFLAG-IN2
               PERFORM MATCHING
      *>          INITIALIZE ERRORS
      *>          PERFORM PROCESSING
           ELSE
               DISPLAY "PASS0048"
               PERFORM PII-FILE
           END-IF.
       EXIT.

      *>----------ここから各種計算処理----------
       PROCESSING-EMPLOYEE SECTION.
      *従業員番号を支給ファイルのレコード様式にMOVEする
               MOVE PII-NAME TO PAYMENT-NAME.
               MOVE EMP-ID TO PAYMENT-ID.
      *記念日から入社日を引き勤続期間を算出
                COMPUTE LEN = 2013 * 12 + 4 - EMP-YY * 12 - EMP-MM
      *>  算出した勤続期間から支給係数を算出する
               IF (LEN>0) AND (LEN<=36) AND (EMP-STATUS = 0)
               THEN
                   DISPLAY "PASS0049"
                  MOVE 0.05 TO RATE
               ELSE
                IF (LEN>0) AND (LEN<=36) AND (EMP-STATUS) = 1
                THEN
                  DISPLAY "PASS0050"
                  MOVE 0.10 TO RATE
                ELSE
                 IF (LEN>36) AND (LEN<=120) AND (EMP-STATUS = 0)
                 THEN
                  DISPLAY "PASS0051"
                  MOVE 0.10 TO RATE
                 ELSE
                  IF (LEN>36) AND (LEN<=120) AND (EMP-STATUS = 1)
                  THEN
                   DISPLAY "PASS0052"
                   MOVE 0.15 TO RATE
                  ELSE
                   IF (LEN>120) AND (LEN<=240) AND (EMP-STATUS = 0)
                   THEN
                    DISPLAY "PASS0053"
                    MOVE 0.15 TO RATE
                   ELSE
                    IF (LEN>10) AND (LEN<=240)AND (EMP-STATUS = 1)
                    THEN
                     DISPLAY "PASS0054"
                     MOVE 0.20 TO RATE
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

      *>次の従業員番号重複の判定のためにワークに従業員番号を格納
               MOVE EMP-ID TO EMP-ID-COMP.
               MOVE PII-ID TO PII-ID-COMP.
      *作業領域を初期化する
                INITIALIZE PAYMENT-NAME PAYMENT-AMOUNT
                PAYMENT-AMOUNT2 RATE LEN BASIC-RESULT
                BASIC-RESULT2 BASIC-RESULT3 BASIC-RESULT4
                BASIC-RESULT5 ERRORFLAG-IN1 ERRORFLAG-IN2.
       EXIT.

       END PROGRAM SALARY-CALCULATION.
