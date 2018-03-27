       IDENTIFICATION DIVISION.
       program-id. Program1 as "Calculo-Area-Circunferencia".
       AUTHOR.     REGINALDO_FATEC.
      *****************************************
      *DISPLAY FATEC
      *****************************************
       
       environment division.
       configuration section.

       data division.
       
       working-storage section.
       
          01 DADOS.
  		     02 W-Raio  	PIC  9(03)v99.
    	     02 W-Area    	PIC  9(05)V99.
             
 	      01 MENSAGEMS-DE-TELA.
  		     02 MENSA1    	PIC X(50) VALUE "DIGITE O Raio".
    	     02 MENSA2   	PIC X(30) VALUE "FIM DO PROGRAMA".
    	     02 MENSA3    	PIC X(30) VALUE SPACE.
             
	      01 DATA-DO-SISTEMA.
  	         02 ANO       PIC 9(02) VALUE ZEROS.
  		     02 MES       PIC 9(02) VALUE ZEROS.
     	     02 DIA       PIC 9(02) VALUE ZEROS.
             
       SCREEN SECTION.
       
          01 TELA01.
	         02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
	         02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
	         02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
	         02 LINE 02 COLUMN 28 VALUE
     	         "Calcula da Area de um Circulo".
 	         02 LINE 08 COLUMN 15 VALUE "Raio:".
 	         02 LINE 10 COLUMN 15 VALUE "Area:".


       procedure division.

       Inicio.
       
   	   ACCEPT  DATA-DO-SISTEMA FROM DATE.
       DISPLAY ERASE       AT    0101.
       DISPLAY TELA01      AT    0101.
       MOVE    ZEROS       TO    DADOS.
       
       Entrada.
       
       DISPLAY MENSA1 AT 2030.
       ACCEPT W-Raio AT 0821.
	   DISPLAY MENSA3 AT 1830.

       Calcula.      
	   compute w-Area = 3.1416*(w-Raio**2).          
	   Display w-Area AT 1021.

       Finaliza.
	   DISPLAY MENSA2 AT 1830.
	   Stop " ".
	   Stop Run.
 
           goback.

       end program Program1.
