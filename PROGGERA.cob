       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGGERA.
      *-------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 13/04/2024
      *--- OBJETIVOS: LEITURA DE UM ARQUIVO SEQUENCIAL, ONDE PODE
      *----    PESQUISAR TODO OS REGISTROS DOS ARQUIVOS.
      *---------------------------------------------------
      *---------------------------------------------------
      *---- DEFINICAO DE ARQUIVOS
      *----
      *---- ARQUIVO        TIPO I/O         BOOK
      *---- ALUNOS            I             BOKALU
      *----
      *---------------------------------------------------

      *-----------------------------------------------------------------
       ENVIRONMENT                                             DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       CONFIGURATION                                           SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       INPUT-OUTPUT                                            SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       FILE-CONTROL.
      *-----------------------------------------------------------------
           SELECT ALUNOS  ASSIGN TO
                      "C:\CURSOLOGICA\COBOL\Projeto FINAL\ALUNOS.dat"
                  FILE STATUS IS FS-ALUNOS.

      *-----------------------------------------------------------------
       DATA                                                    DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       FILE                                                    SECTION.
      *-----------------------------------------------------------------

       FD  ALUNOS.
       COPY "BOKALU".

      *-----------------------------------------------------------------
       WORKING-STORAGE                                         SECTION.
      *-----------------------------------------------------------------

      *------------- VARIAVEIS DE STATUS
       77  FS-ALUNOS                           PIC 9(02).

      *------------- VARIAVEIS DE MENSAGENS
       COPY "APOIO".

      *-----------------------------------------------------------------

       01  WRK-APOIO.
           05 WRK-SEGURATELA                   PIC X(01) VALUE SPACES.
           05 WRK-OPCAO                        PIC X(01).
           05 WRK-LINHA                        PIC 9(003) VALUE 5.
           05 WRK-PAGINA                       PIC 9(003) VALUE ZEROS.
           05 WRK-QTD-ALUNOSCAD                PIC 9(04)  VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN                                                  SECTION.
      *-----------------------------------------------------------------

       01  SCR-TELA.
           05 BLANK SCREEN.
           05 LINE  01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
           05 LINE  01 COLUMN 32 VALUE "PESQUISA GERAL"
                  BACKGROUND-COLOR 3.

       01  SCR-CABECALHO.
           05 LINE  03 COLUMN 01 VALUE "RM  ".
           05 LINE  03 COLUMN 06 VALUE "NOME".
           05 LINE  03 COLUMN 31 VALUE "CURSO".
           05 LINE  03 COLUMN 51 VALUE "SEMESTRE".
           05 LINE  03 COLUMN 61 VALUE "MENSALIDADE".

       01  SCR-SEPARADOR.
           05 LINE  04 COLUMN 01 VALUE "----".
           05 LINE  04 COLUMN 06 VALUE "--------------- ".
           05 LINE  04 COLUMN 31 VALUE "--------------- ".
           05 LINE  04 COLUMN 51 VALUE "---".
           05 LINE  04 COLUMN 61 VALUE "----- ".

       01  SCR-RODAPE.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

       01  SCR-PAGINA.
           05 LINE  23 COLUMN 60 VALUE "PAGINA : ".
           05 LINE  23 COLUMN 69 FROM  WRK-PAGINA.

       01  SCR-RESULTDADOS.
           05 LINE  22 COLUMN 18 VALUE "ALUNOS CADASTRADOS : ".
           05 LINE  22 COLUMN 45 FROM WRK-QTD-ALUNOSCAD.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------
           INITIALISE ARQ-DADOS WRK-MSG WRK-APOIO.

       0001-PRINCIPAL                          SECTION.

            PERFORM 0100-INICIALIZAR.
            PERFORM 0200-PROCESSAR.
            PERFORM 0300-FINALIZAR.
            GOBACK.

      *-----------------------------------------------------------------
       0100-INICIALIZAR                                        SECTION.
      *-----------------------------------------------------------------

           DISPLAY SCR-TELA.

      *----------- ABERTURA DO ARQUIVO
            OPEN INPUT  ALUNOS.

      *------ ARQUIVO NAO EXISTE
            IF  FS-ALUNOS NOT EQUAL 0
                MOVE WRK-MSG-OPEN             TO WRK-MSG
                 PERFORM 0900-MOSTRA
                  GOBACK
            END-IF.

      *-----------------------------------------------------------------
       0100-INICIALIZAR-99-FIM                                 SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0200-PROCESSAR                                          SECTION.
      *-----------------------------------------------------------------

           DISPLAY SCR-CABECALHO
           DISPLAY SCR-SEPARADOR
           MOVE 5                             TO WRK-LINHA

      *-------------------- AREA DE LEITURA ----------------------------

           READ ALUNOS.

               ADD 1                          TO WRK-PAGINA

               PERFORM UNTIL FS-ALUNOS NOT EQUAL ZEROS

                    DISPLAY SCR-PAGINA
                    DISPLAY  ARQ-RM          LINE WRK-LINHA COLUMN 01
                    DISPLAY  ARQ-NOMEALUNO   LINE WRK-LINHA COLUMN 06
                    DISPLAY  ARQ-CURSO       LINE WRK-LINHA COLUMN 31
                    DISPLAY  ARQ-SEMESTRE    LINE WRK-LINHA COLUMN 51
                    DISPLAY  ARQ-MENSALIDADE LINE WRK-LINHA COLUMN 61

                    READ ALUNOS
                    ADD 1                     TO WRK-LINHA
                    ADD  1                    TO WRK-QTD-ALUNOSCAD
                    IF WRK-LINHA  GREATER 15
                       ADD  1                 TO WRK-PAGINA
                       MOVE 5                 TO WRK-LINHA
                       MOVE "PRESSIONE ENTER" TO WRK-MSG
                       PERFORM 0900-MOSTRA
                       DISPLAY SCR-TELA
                       DISPLAY SCR-CABECALHO
                       DISPLAY SCR-SEPARADOR
                    END-IF

               END-PERFORM.


      *------------------------ APRESENTA A QUANTIDADE DE ALUNOS -------
            DISPLAY SCR-RESULTDADOS.
            MOVE "PRESSIONE ENTER PARA SAIR"  TO WRK-MSG

            PERFORM 0900-MOSTRA.
      *-----------------------------------------------------------------
       0200-PROCESSAR-99-FIM                                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0300-FINALIZAR                                          SECTION.
      *-----------------------------------------------------------------
             CLOSE  ALUNOS.

              IF  FS-ALUNOS NOT EQUAL 0
                MOVE WRK-MSG-CLOSE            TO WRK-MSG
                PERFORM 0900-MOSTRA
              END-IF.

      *-----------------------------------------------------------------
       0300-FINALIZAR-99-FIM                                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0900-MOSTRA                                             SECTION.
      *-----------------------------------------------------------------

           ACCEPT SCR-RODAPE.

      *-----------------------------------------------------------------
       0900-MOSTRA-99-FIM                                      SECTION.
      *-----------------------------------------------------------------
