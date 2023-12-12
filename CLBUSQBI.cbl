
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.



       77 WS-COMIENZO PIC 99 VALUE 0.
       77 WS-IND-ENC  PIC 99 VALUE 0.

       77 WS-FIN      PIC 99 VALUE 0.
       77 WS-MITAD    PIC 99 VALUE 0.
       77 WS-ELEMENTO PIC 99 VALUE 6.
       77 WS-TAM      PIC 99 VALUE 7.



        01 WS-VARIABLE.
           02 WS-VECTOR   PIC 99 OCCURS 7 TIMES
           ASCENDING WS-VECTOR
               INDEXED BY WS-I.


           02 WS-BUSCAR.
              05 SW-ENCONTRO-SEC         PIC X(01) VALUE SPACE.
                 88 SW-ENCONTRO-SEC-NO   VALUE 'N'.
                 88 SW-ENCONTRO-SEC-SI   VALUE 'S'.

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-CARGAR-DATOS
              THRU 1000-CARGAR-DATOS-EXIT.

           PERFORM 2000-BUSCAR-SEC
              THRU 2000-BUSCAR-SEC-EXIT.

           PERFORM 3000-BUSCAR-BI
              THRU 3000-BUSCAR-SEC-EXIT.



           STOP RUN.

      *----------------------------------------------------------------*
       1000-CARGAR-DATOS.

           MOVE 1  TO  WS-VECTOR(1)
           MOVE 4  TO  WS-VECTOR(2)
           MOVE 6  TO  WS-VECTOR(3)
           MOVE 8  TO  WS-VECTOR(4)
           MOVE 10 TO  WS-VECTOR(5)
           MOVE 18 TO  WS-VECTOR(6)
           MOVE 22 TO  WS-VECTOR(7).


       1000-CARGAR-DATOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-BUSCAR-SEC.

           DISPLAY '-------- Busqueda Secuencial  ----------'

           SET SW-ENCONTRO-SEC-NO TO TRUE

           PERFORM  VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > WS-TAM OR SW-ENCONTRO-SEC-SI
              IF WS-VECTOR(WS-I) EQUAL WS-ELEMENTO THEN
                 SET SW-ENCONTRO-SEC-SI TO TRUE
                 MOVE WS-I   TO WS-IND-ENC
              END-IF
           END-PERFORM.

           IF SW-ENCONTRO-SEC-SI THEN
              DISPLAY 'Posicion: ' WS-VECTOR(WS-IND-ENC)
             ' ,Elemento encontrado: ' WS-VECTOR(WS-ELEMENTO)
           ELSE
              DISPLAY 'No se encontro el Elemento: '
              WS-VECTOR(WS-ELEMENTO)
           END-IF.
       2000-BUSCAR-SEC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-BUSCAR-BI.

           DISPLAY '-------- Busqueda Binaria ----------'

           MOVE 1  TO WS-I
           MOVE 1  TO WS-COMIENZO
           MOVE WS-TAM  TO WS-FIN

           SET SW-ENCONTRO-SEC-NO TO TRUE

           PERFORM   UNTIL WS-COMIENZO > WS-FIN
                        OR SW-ENCONTRO-SEC-SI


              ADD 1 TO WS-I
              ADD WS-COMIENZO  TO WS-FIN GIVING WS-MITAD
              DIVIDE  WS-MITAD BY 2      GIVING WS-MITAD

              IF WS-VECTOR(WS-I) EQUAL WS-ELEMENTO THEN

                  SET SW-ENCONTRO-SEC-SI TO TRUE
                  MOVE WS-I   TO WS-IND-ENC

                 ELSE IF WS-VECTOR(WS-MITAD) > WS-ELEMENTO THEN
                      MOVE WS-MITAD TO WS-FIN
                 ELSE
                      MOVE WS-MITAD TO WS-COMIENZO
                 END-IF
              END-IF

           END-PERFORM.

           IF SW-ENCONTRO-SEC-SI THEN
              DISPLAY 'Posicion: ' WS-VECTOR(WS-IND-ENC)
             ' ,Elemento encontrado: ' WS-VECTOR(WS-ELEMENTO)
           ELSE
              DISPLAY 'No se encontro el Elemento: '
              WS-VECTOR(WS-ELEMENTO)
           END-IF.
       3000-BUSCAR-SEC-EXIT.
           EXIT.



       END PROGRAM YOUR-PROGRAM-NAME.
