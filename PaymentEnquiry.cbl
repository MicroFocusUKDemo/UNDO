      $SET CURRENCY-SIGN(36)
       IDENTIFICATION DIVISION.
       program-id. PaymentEnquiry as "PaymentEnquiry".
      ************************************************************************
      *
      *  Copyright (C) Micro Focus 1984-2020. All rights reserved.
      *  All rights reserved.
      *
      ************************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FUNCTION                 PIC XXX.

       01  DISPLAY-INTRATE                PIC Z9.999.
       01  DISPLAY-RATE                PIC Z9.999.
       01  DISPLAY-BASE                PIC Z9.999.
       01  DISPLAY-DISC                PIC ZZ9.999.

      *>>
      *>> Definition of data to pass in/out LOANCALC module
      *>> 
           copy "WSLOANCALC.cpy".

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOUR 2. *> 2-green, 1-blue, 3=cyan, 4-red, 5-purple
                                                                   *> 6-yellow, 7-white

       01  USER-INPUT-SCREEN.
           03                          LINE 1 COLUMN 1 PIC X(79) ALL "-".

           03                          LINE 2 COLUMN 1 PIC X(79) Value
                   " Calculate impact of new monthly payment or discount code".
           03                          LINE 3 COLUMN 1 PIC X(79) Value
                   " Enter the three character discount code and new monthly amount".
           03                          LINE 5 COLUMN 1 PIC X(79) Value
                   " Codes: S05/S10 (Std 5%/10%), B05/B10 (Base 5%/10%), XCO (100% Disc)".
           03                          LINE 6 COLUMN 1 PIC X(79) Value
                   " Demonstration code is XCO, amount 200".

           03                          LINE 8 COLUMN 1 PIC X(79) ALL "=".

           03                          LINE 10 COLUMN 5 VALUE "Discount Code:   [".
           03                          LINE 10 COLUMN 23 PIC X(3) USING discount-code.
           03                          LINE 10 COLUMN 26 VALUE "] Enter discount code or 999 to exit".

           03                          LINE 12 COLUMN 5 VALUE "Payment Amount:         [".
           03                          LINE 12 COLUMN 40 PIC $zzz,zz9.99 USING paymt-amount.
           03                          LINE 12 COLUMN 53 VALUE "]".
           
           03                          LINE 13 COLUMN 1 PIC X(79) ALL "-".

           03                          LINE 14 COLUMN 5 VALUE "Outstanding Balance:    [".
           03                          LINE 14 COLUMN 40 PIC $zzz,zz9.99 FROM outstanding-amount.
           03                          LINE 14 COLUMN 53 VALUE "]".

           03                          LINE 16 COLUMN 5 VALUE "Interest Rate:          [".
           03                          LINE 16 COLUMN 46 PIC X(6) FROM DISPLAY-INTRATE.
           03                          LINE 16 COLUMN 53 VALUE "]".

           03                          LINE 18 COLUMN 5 VALUE "Bank base lending rate: [".
           03                          LINE 18 COLUMN 46 PIC X(6) FROM display-base.
           03                          LINE 18 COLUMN 53 VALUE "]".
           03                          LINE 20 COLUMN 1 PIC X(79) ALL "=".

       01  USER-OUTPUT-SCREEN.
           03                          LINE 1 COLUMN 1 PIC X(79) ALL "-".
           03                          LINE 3 COLUMN 5 VALUE "FINISH: [".
           03                          LINE 3 COLUMN 15 PIC XXX USING WS-FUNCTION.
           03                          LINE 3 COLUMN 19 VALUE "] Enter to go again, 999 to exit".

           03                          LINE 5 COLUMN 5 VALUE "Payment Amount:".
           03                          LINE 5 COLUMN 38 VALUE "[".
           03                          LINE 5 COLUMN 40 PIC $zzz,zz9.99 FROM paymt-amount.
           03                          LINE 5 COLUMN 53 VALUE "]".

           03                          LINE 7 COLUMN 5 VALUE "Outstanding Balance:".
           03                          LINE 7 COLUMN 38 VALUE "[".
           03                          LINE 7 COLUMN 40 PIC $zzz,zz9.99 FROM outstanding-amount.
           03                          LINE 7 COLUMN 53 VALUE "]".

           03                          LINE 9 COLUMN 5 VALUE "Discounted Interest Rate:".
           03                          LINE 9 COLUMN 38 VALUE "[".
           03                          LINE 9 COLUMN 46 PIC X(6) FROM display-rate.
           03                          LINE 9 COLUMN 53 VALUE "]".

           03                          LINE 11 COLUMN 5 VALUE "Interest Discount Percent:".
           03                          LINE 11 COLUMN 38 VALUE "[".
           03                          LINE 11 COLUMN 45 PIC X(7) FROM display-disc.
           03                          LINE 11 COLUMN 53 VALUE "]".

           03                          LINE 13 COLUMN 5 VALUE "Bank base lending rate:".
           03                          LINE 13 COLUMN 38 VALUE "[".
           03                          LINE 13 COLUMN 46 PIC X(6) FROM display-base.
           03                          LINE 13 COLUMN 53 VALUE "]".

           03                          LINE 14 COLUMN 1 PIC X(79) ALL "-".

           03                          LINE 15 COLUMN 5 VALUE "Payment Amount for interest:".
           03                          LINE 15 COLUMN 38 VALUE "[".
           03                          LINE 15 COLUMN 40 PIC $zzz,zz9.99 FROM paymt-interest.
           03                          LINE 15 COLUMN 53 VALUE "]".

           03                          LINE 17 COLUMN 5  VALUE "Payment Amount towards capital:".
           03                          LINE 17 COLUMN 38 VALUE "[".
           03                          LINE 17 COLUMN 40 PIC $zzz,zz9.99 FROM paymt-capital.
           03                          LINE 17 COLUMN 53 VALUE "]".

           03                          LINE 19 COLUMN 5 VALUE "New Outstanding Balance:".
           03                          LINE 19 COLUMN 38 VALUE "[".
           03                          LINE 19 COLUMN 40 PIC $zzz,zz9.99 FROM newtopay-display.
           03                          LINE 19 COLUMN 53 VALUE "]".

           03                          LINE 24 COLUMN 5 PIC X(40) VALUE ALL "=".

           03                          LINE 24 COLUMN 1 PIC X(79) ALL "-".

       PROCEDURE DIVISION.
           INITIALIZE CALCULATOR-FIELDS

      *>> These values would usually come from a customer file,
      *>> but for this example we have hard-coded them
           MOVE 5 TO INTEREST-RATE DISPLAY-INTRATE
           MOVE 0.25  TO BASE-RATE
           MOVE 10000 TO OUTSTANDING-AMOUNT

           PERFORM UNTIL WS-FUNCTION = "999" or DISCOUNT-CODE = "999"
               PERFORM P1000-INPUT-SCREEN
               
               IF DISCOUNT-CODE NOT = "999"
                   CALL "LOANCALC" USING BY REFERENCE CALCULATOR-FIELDS

                   PERFORM P2000-OUTPUT-SCREEN  *> Display impact on the loan

               END-IF
           END-PERFORM

           GOBACK
           .

      *>> 
      *>> Get the data to the formatted screen fields
      *>> 
       P0500-POPULATE-SCREEN SECTION.
           MULTIPLY WS-CALCULATED-RATE BY 100 GIVING DISPLAY-RATE
           MOVE BASE-RATE     TO DISPLAY-BASE
           MOVE DISCOUNT-RATE TO DISPLAY-DISC
           .

      *>> 
      *>> Show the Input Screen and accept code/amount
      *>> 
       P1000-INPUT-SCREEN SECTION.
           PERFORM P0500-POPULATE-SCREEN

           DISPLAY CLEAR-SCREEN
           DISPLAY USER-INPUT-SCREEN
           ACCEPT USER-INPUT-SCREEN
           .

      *>> 
      *>> Show the results of the new payment amount
      *>> 
       P2000-OUTPUT-SCREEN SECTION.
           PERFORM P0500-POPULATE-SCREEN

           DISPLAY CLEAR-SCREEN
           DISPLAY USER-OUTPUT-SCREEN
           ACCEPT USER-OUTPUT-SCREEN
           .

       END PROGRAM PAYMENTENQUIRY.
