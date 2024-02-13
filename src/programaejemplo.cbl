       IDENTIFICATION DIVISION.
       PROGRAM-ID ADCDAP13.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CON-DATOS    ASSIGN TO PERSONA
                  FILE STATUS IS FS-CON-DATOS.
      *
           SELECT COPIA        ASSIGN TO COPIA
                  FILE STATUS IS FS-COPIA.
      *
       DATA DIVISION.
       FILE SECTION.
       FD CON-DATOS
           RECORDING MODE IS F.
       01 REG-CON-DATOS          PIC X(10).
      *
       FD COPIA
           RECORDING MODE IS F.
       01 REG-COPIA              PIC X(10).
      *
       WORKING-STORAGE SECTION.
      ******************************************************************
      *                     V A R I A B L E S                          *
      ******************************************************************
       01 FS-CON-DATOS           PIC 99.
       01 
FS-COPIA               PIC 99.
      *
       01 DETALLE.
          05 GENERO              PIC X.
          05 EDAD                PIC 99.
          05 TIPO-DOC            PIC XXX.
          05 FECHA.
             10 SIGLO            PIC XX.
             10 ANNO             PIC XX.
      *
       01 VARIABLES.
          05 VA-CONT             PIC 99.
          05 VA-EDAD             PIC 999.
          05 VA-PROM             PIC 99.
          05 VA-CONTM            PIC 99.
          05 VA-EDADM            PIC 999.
          05 VA-PROMM            PIC 99.
      *
      ******************************************************************
      *                        S W I T C H E S                         *
      ******************************************************************
       01 SWITCH.
          05 SWITCH-FIN          PIC XX VALUE 'NO'.
             88 FIN-OK                  VALUE 'SI'.
             88 FIN-NO                  VALUE 'NO'.
      *
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.
       INDICE.
           PERFORM 10-INICIO
           PERFORM 20-PROCESO
           PERFORM 30-FIN.
      *
      ******************************************************************
      *  10-INICIO                                                   *
      *                                                                *
      ******************************************************************
       10-INICIO.
           INITIALIZE  SWITCH-FIN
                       VARIABLES
      *
           OPEN  INPUT  CON-DATOS
                 OUTPUT COPIA
      *
           IF FS-CON-DATOS NOT = 0
              DISPLAY 'ERROR FILE STATUS AL ABRIR F-E: ' FS-CON-DATOS
              STOP RUN
           END-IF
      *
           IF FS-COPIA NOT = 0
              DISPLAY 'ERROR FILE STATUS AL ABRIR F-S: ' FS-COPIA
              STOP RUN
           END-IF
      *
           PERFORM 100-LEER-FICHERO
      *
           IF FIN-OK
              DISPLAY 'FINALIZO EL FICHERO'
              PERFORM 30-FIN
           END-IF
           .
      *
      ******************************************************************
      *  100-LEER-FICHERO                                              *
      *                                                                *
      ******************************************************************
       100-LEER-FICHERO.
           READ CON-DATOS INTO DETALLE
           IF  FS-CON-DATOS  NOT = 0 AND 10
               DISPLAY 'ERROR AL LEER F-E: ' FS-CON-DATOS
               PERFORM 30-FIN
           END-IF
      *
           IF FS-CON-DATOS = 10
              SET FIN-OK TO TRUE
           END-IF
           .
      ******************************************************************
      *  20-PROCESO                                                   *
      *                                                                *
      ******************************************************************
       20-PROCESO.
           PERFORM 200-TRATAR UNTIL FIN-OK
           PERFORM 2000-ESTADISTICA
           .
      ******************************************************************
      *  1000-INICIO                                                   *
      *                                                                *
      ******************************************************************
      *
       200-TRATAR.
           EVALUATE GENERO
           WHEN 'H'
                COMPUTE VA-CONT = VA-CONT + 1
                COMPUTE VA-EDAD = VA-EDAD + EDAD
           WHEN 'M'
                COMPUTE VA-CONTM = VA-CONTM + 1
                COMPUTE VA-EDADM = VA-EDADM + EDAD
           WHEN OTHER
                DISPLAY 'GENERO: NO CORRESPONDE A NINGUNO'
           END-EVALUATE

           WRITE REG-COPIA FROM REG-CON-DATOS
      *
           IF FS-COPIA NOT = 0
              DISPLAY 'ERROR AL ESCRIBIR F-S: ' FS-COPIA
              STOP RUN
           END-IF
      *
           PERFORM 100-LEER-FICHERO

      *    IF FS-CON-DATOS = 10 THEN
      *       SET FIN-OK TO TRUE
      *    END-IF
           .
      ******************************************************************
      *  2000-ESTADISTICA                                              *
      *                                                                *
      ******************************************************************
       2000-ESTADISTICA.
           COMPUTE VA-PROM = VA-EDAD / VA-CONT
           COMPUTE VA-PROMM = VA-EDADM / VA-CONTM

           DISPLAY 'SE LEYERON EN EL FICHERO TANTOS HOMBRES: ' VA-CONT
           DISPLAY 'LA SUMA DE LA EDAD EN HOMBRES ES: ' VA-EDAD
           DISPLAY 'EL PROMEDIO EN HOMBRES ES: ' VA-PROM

           DISPLAY 'SE LEYERON EN EL FICHERO TANTAS MUJERES: ' VA-CONTM
           DISPLAY 'LA SUMA DE LA EDAD EN MUJERES ES: ' VA-EDADM
           DISPLAY 'EL PROMEDIO EN MUJERES ES: ' VA-PROMM
           .
      *
      ******************************************************************
      *  30-FIN                                                        *
      *                                                                *
      ******************************************************************
       30-FIN.
           CLOSE CON-DATOS
      *
           IF FS-CON-DATOS NOT = 0
              DISPLAY 'ERROR FILE STATUS AL CERRAR F-E: ' FS-CON-DATOS
              STOP RUN
           END-IF
      *
           IF FS-COPIA NOT = 0
              DISPLAY 'ERROR FILE STATUS AL CERRAR F-S: ' FS-COPIA
              STOP RUN
           END-IF
      *
           STOP RUN
           .
      *
      ******************************************************************
      *  30-FIN                                                        *
      *                                                                *
      ******************************************************************