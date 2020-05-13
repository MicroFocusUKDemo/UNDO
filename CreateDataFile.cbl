       identification division.
       program-id. CreateDataFile as "CreateDataFile".                               

       copy "SDDiscRate.cpy".

       DATA DIVISION.
       File Section.
           FD discFile IS EXTERNAL.
           copy "RDDiscRate.cpy" REPLACING ==(PREFIX)== BY ==FILE==.    

       WORKING-STORAGE SECTION.
           copy "RDDiscRate.cpy" REPLACING ==(PREFIX)== BY ==WS-B==.    

       01  WS-FILE-STATUS              PIC XX.

       PROCEDURE DIVISION.
           INITIALIZE FILE-discount-rec

           OPEN OUTPUT discFile.

           MOVE "S10"  to  FILE-code
           move 10     to  FILE-rate
           move "31/12/9999" to FILE-exp-date 
           WRITE FILE-discount-rec

           MOVE "B10"  to  FILE-code
           move 10     to  FILE-rate
           move "31/12/9999" to FILE-exp-date 
           WRITE FILE-discount-rec

           MOVE "S05"  to  FILE-code
           move 10     to  FILE-rate
           move "31/12/9999" to FILE-exp-date 
           WRITE FILE-discount-rec

           MOVE "B05"  to  FILE-code
           move 10     to  FILE-rate
           move "31/12/9999" to FILE-exp-date 
           WRITE FILE-discount-rec

           MOVE "XCO"  to  FILE-code
           move -100    to  FILE-rate
           move "31/07/2020" to FILE-exp-date 
           WRITE FILE-discount-rec

           CLOSE discFile.

       end program CreateDataFile.
