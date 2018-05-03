       program-id. FIBONACCI as "FIBONACCI".
       author. Reginaldo
       date-written. 02/05/2018.

      *----------------------------------------------------------------*
      * inicio: a = 0 b = 1 c = 0
      *
      *      c = a + b       move a = b e b = c
      *  1°  c = 0 + 1 = 1        a = 1   b = 1
      *  2°  c = 1 + 1 = 2        a = 1   b = 2
      *  3°  c = 1 + 2 = 3        a = 2   b = 3
      *  4°  c = 2 + 3 = 5        a = 3   b = 5
      *  5°  c = 3 + 5 = 8        a = 5   b = 8
      *  6°  c = 5 + 8 = 13       a = 8   b = 13 
      *
      *----------------------------------------------------------------*

      *================================================================*
       environment division.
      *================================================================*

       special-names.
          decimal-point is comma.

      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 VARIAVEIS-NUM.
           02 VALOR-ENTRADA                  PIC 9(02)     VALUE ZEROS.
           02 MASC-ENTRADA                   PIC Z9        VALUE ZEROS.
           02 VALORA                         PIC 9(06)     VALUE ZEROS.
           02 VALORB                         PIC 9(06)     VALUE 1.
           02 VALORC                         PIC 9(06)     VALUE ZEROS.
           02 RESULTADO1                     PIC Z9,B      VALUE ZEROS.
           02 RESULTADO2                     PIC ZZ9,B     VALUE ZEROS.
           02 RESULTADO3                     PIC Z.ZZ9,B   VALUE ZEROS.
           02 RESULTADO4                     PIC ZZ.ZZ9,B  VALUE ZEROS.
           02 RESULTADO5                     PIC ZZZ.ZZ9,B VALUE ZEROS.
           02 CONTADOR                       PIC 9(02)     VALUE 2.
           02 COLUNA                         PIC 9(02)     VALUE 9.
           02 COLUNA2                        PIC 9(02)     VALUE 4.

       01 VARIAVEIS-ALFA.
           02 CONTINUA                       PIC X(01)     VALUE SPACES.

       01 MENSAGENS.
           02 MENS01                         PIC X(30)     VALUE SPACES.


       01 DATA-DO-SISTEMA.
           02 ANO                            PIC 9(04)     VALUE ZEROS.
           02 MES                            PIC 9(02)     VALUE ZEROS.
           02 DIA                            PIC 9(02)     VALUE ZEROS.


      *----------------------------------------------------------------*
       screen section.
      *----------------------------------------------------------------*

       01 TELA01.
           02 LINE 02 COLUMN 05              PIC 9(02)/    USING DIA.
           02 LINE 02 COLUMN 08              PIC 9(02)/    USING MES.
           02 LINE 02 COLUMN 11              PIC 9(04)     USING ANO.
           02 LINE 02 COLUMN 25  VALUE "SEQUENCIA DE FIBONACCI.".
           02 LINE 05 COLUMN 02  VALUE 
                "ESCOLHA O NUMERO DA SEQUENCIA (0 a 30): ".
           02 LINE 07 COLUMN 02  VALUE
                "RESULTADO.: ".  
           02 LINE 11 COLUMN 02  VALUE "DESEJA CONTINUAR (S/N): < >".  



      *================================================================*
       procedure division.
      *================================================================*

            ACCEPT DATA-DO-SISTEMA FROM DATE YYYYMMDD.
            PERFORM PROCESSO UNTIL CONTINUA = "N" OR "n".
            PERFORM FINALIZA.        



       PROCESSO.
            PERFORM MOSTRA-TELA
            PERFORM ENTRA-DADOS
            PERFORM MOSTRA-RESULTADO
            PERFORM CONTINUAR
            EXIT.



       MOSTRA-TELA.
            DISPLAY    ERASE               AT          0101
            DISPLAY    TELA01              AT          0101
            EXIT.        


       ENTRA-DADOS.
            MOVE       ZEROS                 TO         VARIAVEIS-NUM
            MOVE       1                     TO         VALORB
            MOVE       2                     TO         CONTADOR
            MOVE       9                     TO         COLUNA
            MOVE       4                     TO         COLUNA2
            ACCEPT     MASC-ENTRADA          AT   0542 WITH PROMPT AUTO
            MOVE       MASC-ENTRADA          TO         VALOR-ENTRADA
            IF         VALOR-ENTRADA         >          30
            DISPLAY    "NUMERO INVALIDO!!"   AT         0642
            PERFORM    ENTRA-DADOS
            ELSE
            DISPLAY    MENS01                AT         0642 
            END-IF
            EXIT.
 


       CALCULA.
            COMPUTE    VALORC = VALORA + VALORB
            COMPUTE    VALORA = VALORB
            COMPUTE    VALORB = VALORC

            COMPUTE    CONTADOR = CONTADOR + 1

            EVALUATE   VALORC
            WHEN       1 THRU 99
            MOVE       VALORC                TO         RESULTADO1
            DISPLAY    RESULTADO1            LINE 08    COLUMN COLUNA
            COMPUTE    COLUNA = COLUNA + 4
            WHEN       100 THRU 999
            MOVE       VALORC                TO         RESULTADO2
            DISPLAY    RESULTADO2            LINE 08    COLUMN COLUNA
            COMPUTE    COLUNA = COLUNA + 5
            WHEN       1000 THRU 9999
            MOVE       VALORC                TO         RESULTADO3
            DISPLAY    RESULTADO3            LINE 08    COLUMN COLUNA
            COMPUTE    COLUNA = COLUNA + 7
            WHEN       10000 THRU 99999
            MOVE       VALORC                TO         RESULTADO4
            DISPLAY    RESULTADO4            LINE 09    COLUMN COLUNA2
            COMPUTE    COLUNA2 = COLUNA2 + 8
            WHEN       10000 THRU 999999
            MOVE       VALORC                TO         RESULTADO5
            DISPLAY    RESULTADO5            LINE 09    COLUMN COLUNA2
            COMPUTE    COLUNA2 = COLUNA2 + 9
            END-EVALUATE
            EXIT.
            
 

       MOSTRA-RESULTADO.
            EVALUATE   VALOR-ENTRADA
            WHEN 0
            DISPLAY    "{ 0"                AT         0802
            MOVE       ZEROS                TO         CONTADOR
            WHEN 1
            DISPLAY    "{ 0, 1"             AT         0802
            MOVE       1                    TO         CONTADOR
            WHEN OTHER
            DISPLAY    "{ 0, 1, "           AT         0802
            PERFORM    CALCULA UNTIL  CONTADOR > VALOR-ENTRADA
            END-EVALUATE

            EVALUATE   CONTADOR
            WHEN       0
            COMPUTE    COLUNA = COLUNA - 3
            DISPLAY    "}"            LINE 08    COLUMN COLUNA
            WHEN       1
            COMPUTE    COLUNA = COLUNA - 1
            DISPLAY    " }"           LINE 08    COLUMN COLUNA
            WHEN       2 THRU 20 
            COMPUTE    COLUNA = COLUNA - 2
            DISPLAY    " }"           LINE 08    COLUMN COLUNA
            WHEN OTHER
            COMPUTE    COLUNA2 = COLUNA2 - 2
            DISPLAY    " }"           LINE 09    COLUMN COLUNA2
            END-EVALUATE
            EXIT.
       


       CONTINUAR.
            MOVE       SPACES                TO         CONTINUA
            ACCEPT     CONTINUA    AT        1127 WITH PROMPT AUTO
            IF         CONTINUA = "S" OR "s" OR "N" OR "n"
            NEXT SENTENCE
            ELSE
            PERFORM    CONTINUAR
            EXIT.

       FINALIZA.
            DISPLAY   "FIM DO PROGRAMA"      AT         1302
            STOP " "
            STOP RUN
            EXIT.
            

           goback.

       end program FIBONACCI.
