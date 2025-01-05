       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. MENU.
      *-------------------------------------------------
      *---- SOBRE O PROGRAMA: MENU DE NAVEGACAO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 09/04/2024
      *--- OBJETIVOS: ELABORACAO DE UM MENU PARA O PROJETO FINAL
      *----
      *---------------------------------------------------

      *-----------------------------------------------------------------
       ENVIRONMENT                                             DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                                           SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       DATA                                                    DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       WORKING-STORAGE                                         SECTION.
      *-----------------------------------------------------------------

      *-------------------- VARIAVEIS DE APOIO -------------------------

       COPY "APOIO".

      *-----------------------------------------------------------------

       01  WRK-APOIO.
           05 WRK-SEGURATELA                   PIC X(01) VALUE SPACES.
           05 WRK-OPCAO                        PIC X(01).

      *-----------------------------------------------------------------
       SCREEN                                                  SECTION.
      *-----------------------------------------------------------------

       01  SCR-TELA.
           05 BLANK SCREEN.
           05 LINE  01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
           05 LINE  01 COLUMN 32 VALUE "SISTEMA DE CONTROLE"
                  BACKGROUND-COLOR 3.

       01  SCR-MENU.
           05 LINE  07 COLUMN 30 VALUE "1 - CADASTRO".
           05 LINE  08 COLUMN 30 VALUE "2 - CONSULTA POR CODIGO".
           05 LINE  09 COLUMN 30 VALUE "3 - CONSULTA GERAL".
           05 LINE  10 COLUMN 30 VALUE "4 - RELATORIO POR DISCIPLINA".
           05 LINE  11 COLUMN 30 VALUE "5 - RELATORIO GERAL ARQUIVO".
           05 LINE  12 COLUMN 30 VALUE "6 - SAIR".
           05 LINE  15 COLUMN 29 VALUE "ENTRE COM A OPCAO : ".
           05 LINE  15 COLUMN 48 USING WRK-OPCAO.

       01  SCR-RODAPE.
           05 LINE  22 COLUMN 9  VALUE "<<< APERTE ENTER >>> "
                              BACKGROUND-COLOR 2.
           05 LINE  23 COLUMN 9  VALUE "STATUS : "
                              BACKGROUND-COLOR 4.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0001-PRINCIPAL                                          SECTION.
      *-----------------------------------------------------------------

           PERFORM UNTIL WRK-OPCAO EQUAL 6
              DISPLAY SCR-TELA
              ACCEPT  SCR-MENU

              EVALUATE WRK-OPCAO

                WHEN 1
                  CALL "PROGRV01"
                  MOVE SPACES                 TO WRK-OPCAO

                WHEN 2
                  CALL "PROGPESQ"
                  MOVE SPACES                 TO WRK-OPCAO

                WHEN 3
                    CALL "PROGGERA"
                    MOVE SPACES               TO WRK-OPCAO

                WHEN 4
                    CALL "PROGREL1"
                    MOVE SPACES               TO WRK-OPCAO

                WHEN 5
                    CALL "PROGREL2"
                    MOVE SPACES               TO WRK-OPCAO

                WHEN 6
                    MOVE WRK-MSG-OPCAO-AGRD   TO WRK-MSG
                    PERFORM 0900-MOSTRA

                WHEN OTHER
                    MOVE WRK-MSG-OPCAO-ERRO   TO WRK-MSG
                    PERFORM 0900-MOSTRA
                    MOVE SPACES TO WRK-OPCAO

              END-EVALUATE

           END-PERFORM.

           STOP RUN.

      *-----------------------------------------------------------------
       0001-PRINCIPAL-99-FIM                                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0900-MOSTRA                                             SECTION.
      *-----------------------------------------------------------------

           ACCEPT SCR-RODAPE.

      *-----------------------------------------------------------------
       0900-MOSTRA-99-FIM                                      SECTION.
      *-----------------------------------------------------------------
