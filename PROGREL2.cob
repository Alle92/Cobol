       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGREL2.
      *-------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 14/04/2024
      *--- OBJETIVOS: PESQUISAR GERAL DE ALUNOS CADASTRADOS
      *----
      *---------------------------------------------------
      *---------------------------------------------------
      *---- DEFINICAO DE ARQUIVOS
      *----
      *---- ARQUIVO        TIPO I/O         BOOK
      *---- ALUNOS            I             BOKALU
      *---- RELGERAL          O              ----

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

           SELECT RELGERAL ASSIGN TO
                      "C:\CURSOLOGICA\COBOL\Projeto FINAL\RELGERAL.dat"
                  FILE STATUS IS FS-RELGERAL.

      *-----------------------------------------------------------------
       DATA                                                    DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       FILE                                                    SECTION.
      *-----------------------------------------------------------------

      *-------------------- AREA DO ARQUIVO NDE ALUNOS -----------------
       FD  ALUNOS.
       COPY "BOKALU".

      *-------------------- AREA PARA GRAVAR O RELATORIO DOS ALUNOS ----

       FD  RELGERAL.

       01  ARQ-RELGERAL                 PIC X(47).

      *-----------------------------------------------------------------
       WORKING-STORAGE                                         SECTION.
      *-----------------------------------------------------------------

      *------------- VARIAVEIS DE STATUS -------------------------------
       77  FS-ALUNOS                    PIC 9(02).
       77  FS-RELGERAL                  PIC 9(02).

      *------------- VARIAVEL DE WRK-APOIO -----------------------------
       77  WRK-QTD-ALUNOSCAD            PIC 9(04)  VALUE ZEROS.
       77  WRK-OPCAO                    PIC X(01)  VALUE SPACES.

      *------------- VARIAVEIS DE MENSAGENS ----------------------------
       COPY "APOIO".

      *-----------------------------------------------------------------

       01  WRK-APOIO.
           05 WRK-SEGURATELA                   PIC X(01) VALUE SPACES.

      *-----------------------------------------------------------------
       SCREEN                                                  SECTION.
      *-----------------------------------------------------------------

       01  SCR-TELA.
           05 BLANK SCREEN.
           05 LINE  01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
           05 LINE  01 COLUMN 32 VALUE "RELATORIO GERAL DE ALUNOS"
                  BACKGROUND-COLOR 3.

       01  SCR-DADOS.
           05 LINE  05 COLUMN 30 VALUE "DESEJA GERAR O RELATORIO ? S/N".
           05 LINE  05 COLUMN 62 USING WRK-OPCAO.

       01  SCR-REGISTROS.
           05 LINE  10 COLUMN 30 VALUE "REGISTROS TOTAIS : ".
           05 LINE  10 COLUMN 50 FROM  WRK-QTD-ALUNOSCAD.

       01  SCR-RODAPE.
           05 LINE  22 COLUMN 18 VALUE "APERTE QUALQUER BOTAO P/ SAIR"
                              BACKGROUND-COLOR 3.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

       01  SCR-RODAPEREP.
      *     05 LINE  22 COLUMN 18 VALUE "APERTE QUALQUER BOTAO P/ SAIR"
      *                        BACKGROUND-COLOR 3.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------

           INITIALISE WRK-QTD-ALUNOSCAD, WRK-OPCAO.

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

      *----------- ABERTURA DO ARQUIVO
            OPEN OUTPUT RELGERAL.

      *-----------------------------------------------------------------
       0100-INICIALIZAR-99-FIM                                 SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0200-PROCESSAR                                          SECTION.
      *-----------------------------------------------------------------

      *--------------- AREA DE INSERCAO DE DADOS -----------------------
           ACCEPT SCR-DADOS.

           IF WRK-OPCAO EQUAL 'S' OR WRK-OPCAO EQUAL 's'

      *-------------------- AREA DE LEITURA ----------------------------
              READ ALUNOS

               PERFORM UNTIL FS-ALUNOS NOT EQUAL ZEROS

                 MOVE  ARQ-DADOS              TO  ARQ-RELGERAL
                 WRITE ARQ-RELGERAL
                 ADD 1                        TO  WRK-QTD-ALUNOSCAD

                 READ ALUNOS

               END-PERFORM

               DISPLAY SCR-REGISTROS
               MOVE WRK-REGISTRO              TO WRK-MSG
               PERFORM 0900-MOSTRA
           ELSE
               IF  WRK-OPCAO EQUAL 'N' OR WRK-OPCAO EQUAL 'n'
                   PERFORM 0300-FINALIZAR
                   MOVE WRK-SEMCONFIRM        TO WRK-MSG
                   PERFORM 0900-MOSTRA
                   GOBACK
           ELSE
               MOVE WRK-MSG-OPCAO-ERRO        TO WRK-MSG
               DISPLAY SCR-RODAPEREP
               GO  TO 0200-PROCESSAR
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

              CLOSE  RELGERAL.

              IF  FS-RELGERAL NOT EQUAL 0
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
