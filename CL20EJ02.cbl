      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-EMPLEADOS
           ASSIGN TO '../EMPLEADOS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-EMPLEADOS
           RECORD KEY IS ENT-EMP-ID-EMPLEADO.

       SELECT SAL-EMPLEADOS
           ASSIGN TO '../SAL-EMPLEADOS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-EMPLEADOS-SAL
           RECORD KEY IS SAL-EMP-ID-EMPLEADO.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 ENT-EMPLEADOS-REG.
          05 ENT-EMP-ID-EMPLEADO            PIC 9(08).
          05 ENT-EMP-APELLIDO               PIC X(25).
          05 ENT-EMP-NOMBRE                 PIC X(25).
          05 ENT-EMP-ESTADO                 PIC X(01).
          05 ENT-EMP-DIRECCION              PIC X(50).
          05 ENT-EMP-COD-POSTAL             PIC 9(04).

       FD SAL-EMPLEADOS.
       01 SAL-EMPLEADOS-REG.
          05 SAL-EMP-ID-EMPLEADO            PIC 9(08).
          05 SAL-EMP-APELLIDO               PIC X(25).
          05 SAL-EMP-NOMBRE                 PIC X(25).
          05 SAL-EMP-ESTADO                 PIC X(01).
          05 SAL-EMP-DIRECCION              PIC X(50).
          05 SAL-EMP-COD-POSTAL             PIC 9(04).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-CLAVE-NFD          VALUE '23'.

       01 FS-STATUS-SAL.
          05 FS-EMPLEADOS-SAL                   PIC X(2).
             88 FS-EMPLEADOS-SAL-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-SAL-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-SAL-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-SAL-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-SAL-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-SAL-CLAVE-NFD          VALUE '23'.

       77 WS-ID-EMPLEADO                    PIC 9(08).
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-FILE-OK

               DISPLAY 'INGRESA UN ID-EMPLEADO:'
               ACCEPT WS-ID-EMPLEADO
               DISPLAY " "

              PERFORM 2000-BUSCAR-EMPLEADO
                 THRU 2000-BUSCAR-EMPLEADO-FIN

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.
           PERFORM 1200-ABRIR-EMPLEADOS-SAL
              THRU 1200-ABRIR-EMPLEADOS-SAL-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-EMPLEADOS-SAL.

           OPEN OUTPUT SAL-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-SAL-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-SAL-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS-SAL
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS-SAL
           END-EVALUATE.

       1200-ABRIR-EMPLEADOS-SAL-FIN.
           EXIT.

       2000-BUSCAR-EMPLEADO.

           MOVE WS-ID-EMPLEADO          TO ENT-EMP-ID-EMPLEADO.

           PERFORM 2100-LEER-EMPLEADOS
              THRU 2100-LEER-EMPLEADOS-FIN.

       2000-BUSCAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS KEY IS ENT-EMP-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    PERFORM 2105-MOSTRAR-DATOS
                       THRU 2105-MOSTRAR-DATOS-FIN
               WHEN FS-EMPLEADOS-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
               WHEN FS-EMPLEADOS-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
               WHEN FS-EMPLEADOS-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

        2100-LEER-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2105-MOSTRAR-DATOS.

           DISPLAY "--------------------------------------------------".

           DISPLAY " ID EMPLEADO: " ENT-EMP-ID-EMPLEADO.
           DISPLAY " APELLIDO   : " ENT-EMP-APELLIDO.
           DISPLAY " NOMBRE     : " ENT-EMP-NOMBRE.
           DISPLAY " ESTADO     : " ENT-EMP-ESTADO.
           DISPLAY " DIRECCION  : " ENT-EMP-DIRECCION.
           DISPLAY " COD. POSTAL: " ENT-EMP-COD-POSTAL.

           DISPLAY "--------------------------------------------------".

           MOVE ENT-EMPLEADOS-REG TO SAL-EMPLEADOS-REG

           IF ENT-EMP-ESTADO EQUAL "A"

               WRITE SAL-EMPLEADOS-REG



               EVALUATE TRUE
                 WHEN FS-EMPLEADOS-SAL-FILE-OK
                    DISPLAY "SE GRABO EL REGISTRO DE EMPLEADO"
                 WHEN FS-EMPLEADOS-SAL-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
                 WHEN FS-EMPLEADOS-SAL-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
                 WHEN FS-EMPLEADOS-SAL-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
                 WHEN OTHER
                    DISPLAY 'ERROR AL ESCRIBIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS-SAL
               END-EVALUATE

           ELSE

             DISPLAY "EL EMPLEADO ESTA DADO DE BAJA"

           END-IF.
       2105-MOSTRAR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

           DISPLAY " ".
           DISPLAY '### FIN DEL PROGRAMA ###'.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS.

           IF NOT FS-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           CLOSE SAL-EMPLEADOS.

           IF NOT FS-EMPLEADOS-SAL-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS-SAL
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20EJ02.
