       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-INPUT PIC A(1).
       01 CURR-GUESS PIC 9(3).
       01 HIGH PIC 9(3) VALUE 100.
       01 LOW PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       A-PARA.
           PERFORM B-PARA WITH TEST AFTER UNTIL USER-INPUT = 'q' OR 'e'.
           STOP RUN.

       B-PARA.
           COMPUTE CURR-GUESS = (LOW + HIGH) / 2.
           DISPLAY "This program guesses your random number between 1 and 100".
           DISPLAY "Enter 'g' if your number is greater, 'l' if it's less and 'e' if it's equal".
           DISPLAY "You may also enter 'q' to quit at any time.".
           DISPLAY "We guess: "CURR-GUESS.
           ACCEPT USER-INPUT.
           EVALUATE USER-INPUT
                   WHEN 'g'
                           COMPUTE LOW = CURR-GUESS
                   WHEN 'l'
                           COMPUTE HIGH = CURR-GUESS
                   WHEN 'e'
                           DISPLAY "So, "CURR-GUESS", huh?"
                   WHEN 'q'
                           DISPLAY "Leaving? See you!"
                   WHEN OTHER
                           PERFORM B-PARA
           END-EVALUATE.
