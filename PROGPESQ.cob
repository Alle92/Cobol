       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGPESQ.
      *-------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 13/04/2024
      *--- OBJETIVOS: LEITURA DE UM ARQUIVO SEQUENCIAL, ONDE PODE
      *----    PESQUISAR UM REGISTRO POR VEZ.
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

      *------------ VARIAVEIS DE APOIO AO ARQUIVO

       01  WRK-TEMP-DADOS.
           05 WRK-TEMP-RM               PIC X(04) VALUE SPACES.
           05 WRK-TEMP-NOMEALUNO        PIC X(20) VALUE SPACES.
           05 WRK-TEMP-CURSO            PIC X(15) VALUE SPACES.
           05 WRK-TEMP-SEMESTRE         PIC X(02) VALUE SPACES.
           05 WRK-TEMP-MENSALIDADE      PIC 9(06) VALUE ZEROS.


      *------------- VARIAVEIS DE STATUS
       77  FS-ALUNOS                    PIC 9(02).

      *------------- VARIAVEL DE WRK-APOIO
       77  WRK-FLAG-ENCONTRADO          PIC 9(01)  VALUE ZEROS.

      *------------- VARIAVEIS DE MENSAGENS
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
           05 LINE  01 COLUMN 32 VALUE "PESQUISA DE ALUNO"
                  BACKGROUND-COLOR 3.

       01  SCR-DADOS.
           05 LINE  07 COLUMN 30 VALUE "RM...............".
           05 LINE  07 COLUMN 60 USING WRK-TEMP-RM.
           05 LINE  08 COLUMN 30 VALUE "NOME ALUNO.......".
           05 LINE  08 COLUMN 60 FROM  ARQ-NOMEALUNO.
           05 LINE  09 COLUMN 30 VALUE "CURSO............".
           05 LINE  09 COLUMN 60 FROM  ARQ-CURSO.
           05 LINE  10 COLUMN 30 VALUE "SEMESTRE.........".
           05 LINE  10 COLUMN 60 FROM  ARQ-SEMESTRE.
           05 LINE  11 COLUMN 30 VALUE "MENSALIDADE......".
           05 LINE  11 COLUMN 60 FROM  ARQ-MENSALIDADE.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.

       01  SCR-RODAPE.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.
           05 LINE  24 COLUMN 18 VALUE "PRESSIONE ENTER P/ SAIR"
                              BACKGROUND-COLOR 3.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------
           INITIALISE WRK-TEMP-DADOS ARQ-DADOS WRK-MSG WRK-APOIO.

       0001-PRINCIPAL                          SECTION.

            PERFORM 0100-INICIALIZAR.
            PERFORM 0200-PROCESSAR.
            PERFORM 0300-FINALIZAR.
            GOBACK.

      *-----------------------------------------------------------------
       0100-INICIALIZAR                                        SECTION.
      *-----------------------------------------------------------------

      *------------------ APRESENTACAO DE TELAS ------------------------
           MOVE WRK-VAR-SAIDA                 TO WRK-MSG
           DISPLAY SCR-TELA.

      *----------- ABERTURA DO ARQUIVO ---------------------------------
            OPEN INPUT  ALUNOS.

      *------ ARQUIVO NAO EXISTE ---------------------------------------
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

      *--------------- AREA DE INSERCAO DE DADOS -----------------------
           ACCEPT SCR-DADOS.

      *-------------------- AREA DE VERIFICACAO ------------------------
           IF WRK-TEMP-RM  EQUAL '9999'
               PERFORM 0300-FINALIZAR
              GOBACK
           END-IF.

           IF WRK-TEMP-RM EQUAL SPACES
               MOVE "INSIRA UM REGISTRO"      TO WRK-MSG
               PERFORM 0900-MOSTRA
               GO TO 0200-PROCESSAR
           END-IF

      *-------------------- AREA DE LEITURA ----------------------------

               READ ALUNOS
               PERFORM UNTIL FS-ALUNOS NOT EQUAL ZEROS

                  IF WRK-TEMP-RM EQUAL ARQ-RM
                    MOVE 1                    TO WRK-FLAG-ENCONTRADO
                    DISPLAY SCR-DADOS
                    MOVE  WRK-MSG-REG-ENCONTRADO
                                              TO WRK-MSG
                    PERFORM 0900-MOSTRA
                  END-IF
                  READ ALUNOS
               END-PERFORM.

           IF WRK-FLAG-ENCONTRADO EQUAL 1
              DISPLAY SCR-RODAPE
           ELSE
               MOVE WRK-MSG-REG-NAOENCONT     TO WRK-MSG
               PERFORM 0900-MOSTRA
           END-IF.

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
