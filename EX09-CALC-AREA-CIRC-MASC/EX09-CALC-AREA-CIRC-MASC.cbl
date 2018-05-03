       program-id.EX09_CALC_AREA_CIRC_MASC as 
                        "EX09_CALC_AREA_CIRC_MASC".

       author. Grupo 04: Alexsandro Neri, Brunno Melo, Reginaldo Luiz,
                         Rômulo Vannuchi e Walter Chamorro.

       date-written. 14/04/2018.

      ******************************************************************
      * 9)	Programa Aula3 - Calcular a área de uma circunferência
      *                      com máscara.                 
      *                 
      ******************************************************************
      
      *================================================================*
       environment division.
      *================================================================*
       
       special-names.

           decimal-point is comma.



      *----------------------------------------------------------------* 
       working-storage section.
      *----------------------------------------------------------------*
 
       01 DADOS.
  		     02 W-Raio  	       PIC  9(03)V99     VALUE ZEROS.
             02 W-MASC-RAIO        PIC  ZZ9,99       VALUE ZEROS.
    	     02 W-Area    	       PIC  9(05)V99     VALUE ZEROS.
             02 W-MASC-AREA        PIC  ZZ.ZZ9,99    VALUE ZEROS.

       01 CONTINUAR.
             02 CONTINUA           PIC X(01)         VALUE ZEROS.
            
             
 	   01 MENSAGEMS-DE-TELA.
  		     02 MENSA1    	PIC X(50) VALUE "DIGITE O Raio".
    	     02 MENSA2   	PIC X(30) VALUE "FIM DO PROGRAMA".
    	     02 MENSA3    	PIC X(30) VALUE SPACE.

             
	   01 DATA-DO-SISTEMA.
  	         02 ANO       PIC 9(04) VALUE ZEROS.
  		     02 MES       PIC 9(02) VALUE ZEROS.
     	     02 DIA       PIC 9(02) VALUE ZEROS.
      
      *----------------------------------------------------------------* 
       SCREEN SECTION.
      *----------------------------------------------------------------*
 
       01 TELA01.
	         02 LINE 02 COLUMN 05 PIC 9(02)/   USING DIA.
	         02 LINE 02 COLUMN 08 PIC 9(02)/   USING MES.
	         02 LINE 02 COLUMN 11 PIC 9(04)    USING ANO.
	         02 LINE 02 COLUMN 28 VALUE
     	            "Calcula da Area de um Circulo".
 	         02 LINE 08 COLUMN 15 VALUE "Raio: ".
 	         02 LINE 10 COLUMN 15 VALUE "Area: ".
             02 LINE 12 COLUMN 15 VALUE "Deseja Continuar (S/N): < >".
      *----------------------------------------------------------------*

      *================================================================*
       procedure division.
      *================================================================*

       Inicio.
       
   	        ACCEPT  DATA-DO-SISTEMA   FROM DATE  YYYYMMDD.
            DISPLAY  ERASE       AT    0101.
            DISPLAY  TELA01      AT    0101.
            MOVE     ZEROS       TO    DADOS.
       

       Entrada.
       
            DISPLAY    MENSA1       AT   2030.
            ACCEPT     W-MASC-RAIO  AT   0825.
            MOVE       W-MASC-RAIO  TO   W-Raio.
	        DISPLAY    MENSA3       AT   2030.
           

       Calcula.      
	        compute    W-Area = 3,1416 * (W-Raio**2). 
            MOVE       W-Area      TO    W-MASC-AREA.      
	        Display    W-MASC-AREA AT    1022.
         

       FINALIZA.
             ACCEPT        CONTINUA        AT     1240 PROMPT AUTO
             EVALUATE TRUE
             WHEN  CONTINUA = "n" OR "N"
	         DISPLAY MENSA2 AT 1830
	         Stop " "
	         Stop Run
             WHEN  CONTINUA = "S" OR "s"
             MOVE  ZEROS TO DADOS
             MOVE  SPACE TO CONTINUA
             PERFORM Inicio
             PERFORM Entrada
             PERFORM CALCULA
             PERFORM FINALIZA
             WHEN OTHER
             MOVE SPACE TO CONTINUA
             PERFORM FINALIZA
             END-EVALUATE
             EXIT.
 
      
           goback.

       end program EX09_CALC_AREA_CIRC_MASC.
