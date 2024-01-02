      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 20/10/2023
      * Purpose: CLASE 20 - LEER SECUENCIAL ARCHIVO VSAM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20READ.
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
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-EMPLEADOS
           RECORD KEY IS ENT-EMP-ID-EMPLEADO.

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

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-OK                 VALUE '00'.
             88 FS-EMPLEADOS-EOF                VALUE '10'.
             88 FS-EMPLEADOS-NFD                VALUE '35'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-EMPLEADOS          PIC 9(04) VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-EMPLEADOS-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    PERFORM 1110-LEER-EMPLEADOS
                       THRU 1110-LEER-EMPLEADOS-FIN
               WHEN FS-EMPLEADOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    ADD 1                   TO WS-CONT-REG-EMPLEADOS
               WHEN FS-EMPLEADOS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY " "
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
                    DISPLAY " "
           END-EVALUATE.

       1110-LEER-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           DISPLAY 'ID-EMPLEADO: ' ENT-EMP-ID-EMPLEADO ' - '-
                   'APELLIDO: ' ENT-EMP-APELLIDO ' - '-
                   'NOMBRE: ' ENT-EMP-NOMBRE ' - '-
                   'ESTADO: ' ENT-EMP-ESTADO ' - '-
                   'DIRECCION: ' ENT-EMP-DIRECCION ' - '-
                   'COD-POSTAL: ' ENT-EMP-COD-POSTAL.

           PERFORM 1110-LEER-EMPLEADOS
              THRU 1110-LEER-EMPLEADOS-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY "--------------------------------------------------".
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-CONT-REG-EMPLEADOS.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

           DISPLAY '### FIN DEL PROGRAMA ###'.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20READ.
