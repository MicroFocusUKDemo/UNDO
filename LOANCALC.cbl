       IDENTIFICATION DIVISION.
       program-id. LOANCALC.
      ************************************************************************
      *
      *  Copyright (C) Micro Focus 1984-2020. All rights reserved.
      *  All rights reserved.
      *
      ************************************************************************

      * SELECT STATEMENTS FOR FILE USEAGE
           COPY "SDDiscRate.cpy".

       DATA DIVISION.
       FILE SECTION.
           FD DISCFILE IS EXTERNAL.
           COPY "RDDiscRate.cpy" REPLACING ==(PREFIX)== BY ==FILE==.    

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX.
       01  DISPLAY-FILE-STATUS         PIC XX.

       LINKAGE SECTION.
      *>>
      *>> Definition of data to pass in/out LOANCALC module
      *>> 
           copy "WSLOANCALC.cpy".

       PROCEDURE DIVISION USING BY REFERENCE CALCULATOR-FIELDS.
       
       MAIN SECTION.
           INITIALIZE FILE-DISCOUNT-REC

      *>> 
      *>> Use the input discount code to fetchthe rate from the file
      *>> 
           IF DISCOUNT-CODE NOT EQUAL SPACES
               MOVE FUNCTION UPPER-CASE (DISCOUNT-CODE) TO FILE-CODE
               PERFORM P0200-GET-DISCOUNT
           END-IF

      *>> Calculate discounted interest rate
      *>> the Discount rate is applied to the interest rate, therefore
      *>> i.e. 10% discount on a 50% rate gives a new rate of 45%
                 COMPUTE WS-CALCULATED-RATE = INTEREST-RATE
                 * (DISCOUNT-RATE / 100)

      *>> Subtract the caluclated discount
                 COMPUTE WS-CALCULATED-RATE = INTEREST-RATE
                 - WS-CALCULATED-RATE

      *>> If the first character of the code is 'B' add base rate
               IF FILE-CODE(1:1) = "B"
                   COMPUTE WS-CALCULATED-RATE = WS-CALCULATED-RATE
                   + BASE-RATE
               END-IF

      *>> Convert the percentage rate to a factor
                 COMPUTE WS-CALCULATED-RATE = WS-CALCULATED-RATE / 100


      *>> Calculate amount of payment to allocate to interest,
      *>> capital and calculate new balance outstanding
                 COMPUTE PAYMT-INTEREST = OUTSTANDING-AMOUNT
                 * (WS-CALCULATED-RATE / 12)

                 COMPUTE PAYMT-CAPITAL = PAYMT-AMOUNT - PAYMT-INTEREST

                 COMPUTE NEWTOPAY-DISPLAY = OUTSTANDING-AMOUNT
                 - PAYMT-CAPITAL

           GOBACK.

      *>> 
      *>> Read the Discount look file using the code entered on the screen
      *>> 
       P0200-GET-DISCOUNT SECTION.
           OPEN INPUT DISCFILE

           MOVE FUNCTION UPPER-CASE (DISCOUNT-CODE) TO FILE-CODE

           READ DISCFILE KEY IS FILE-CODE
           IF WS-FILE-STATUS = ZERO
               MOVE FILE-RATE TO DISCOUNT-RATE
           ELSE
               MOVE WS-FILE-STATUS TO DISPLAY-FILE-STATUS
               MOVE ZERO TO DISCOUNT-RATE
           END-IF

           CLOSE DISCFILE
           .

       END PROGRAM LOANCALC.
