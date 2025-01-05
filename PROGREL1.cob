       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGREL1.
      *-------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 14/04/2024
      *--- OBJETIVOS: PESQUISAR A QUANTIDADE DE ALUNOS POR DISCIPLINA
      *----
      *---------------------------------------------------
      *---------------------------------------------------
      *---- DEFINICAO DE ARQUIVOS
      *----
      *---- ARQUIVO        TIPO I/O         BOOK
      *---- ALUNOS            I             BOKALU
      *---- DISCIPLINA        O              ----
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
           SELECT DISCIPLINA  ASSIGN TO
                     "C:\CURSOLOGICA\COBOL\Projeto FINAL\DISCIPLINA.dat"
                  FILE STATUS IS FS-DISCIPLINA.

      *-----------------------------------------------------------------
       DATA                                                    DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       FILE                                                    SECTION.
      *-----------------------------------------------------------------

       FD  ALUNOS.
       COPY "BOKALU".

       FD  DISCIPLINA.
       01  ARQ-RELDISCIPLINA                   PIC X(47).

      *-----------------------------------------------------------------
       WORKING-STORAGE                                         SECTION.
      *-----------------------------------------------------------------

      *------------ VARIAVEIS DE APOIO AO ARQUIVO

       01  WRK-TEMP-DADOS.
           05 WRK-TEMP-RM                      PIC X(04) VALUE SPACES.
           05 WRK-TEMP-NOMEALUNO               PIC X(20) VALUE SPACES.
           05 WRK-TEMP-CURSO                   PIC X(15) VALUE SPACES.
           05 WRK-TEMP-SEMESTRE                PIC X(02) VALUE SPACES.
           05 WRK-TEMP-MENSALIDADE             PIC 9(06) VALUE ZEROS.


      *------------- VARIAVEIS DE STATUS
       77  FS-ALUNOS                           PIC 9(02).
       77  FS-DISCIPLINA                       PIC 9(02).

      *------------- VARIAVEL DE WRK-APOIO
       77  WRK-FLAG-ENCONTRADO                 PIC 9(01)  VALUE ZEROS.
       77  WRK-QTD-ALUNOSCAD                   PIC 9(04)  VALUE ZEROS.

      *------------- VARIAVEIS DE MENSAGENS
       COPY "APOIO".

      *-----------------------------------------------------------------

       01  WRK-APOIO.
           05 WRK-SEGURATELA                   PIC X(01) VALUE SPACES.
           05 WRK-OPCAO                        PIC X(01).
           05 WRK-LINHA                        PIC 9(003) VALUE 6.
           05 WRK-PAGINA                       PIC 9(003) VALUE ZEROS.

      *-----------------------------------------------------------------
       SCREEN                                                  SECTION.
      *-----------------------------------------------------------------

       01  SCR-TELA.
           05 BLANK SCREEN.
           05 LINE  01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
           05 LINE  01 COLUMN 32 VALUE "PESQUISA DE CURSO"
                  BACKGROUND-COLOR 3.

       01  SCR-DADOS.
           05 LINE  03 COLUMN 31 VALUE "CURSO............".
           05 LINE  03 COLUMN 61 USING WRK-TEMP-CURSO.
       01  SCR-RESULTDADOS.
           05 LINE  22 COLUMN 31 VALUE "ALUNOS CADASTRADOS : ".
           05 LINE  22 COLUMN 61 FROM WRK-QTD-ALUNOSCAD.

       01  SCR-CABECALHO.
           05 LINE  04 COLUMN 02 VALUE "RM  ".
           05 LINE  04 COLUMN 07 VALUE "NOME".
           05 LINE  04 COLUMN 31 VALUE "CURSO".
           05 LINE  04 COLUMN 51 VALUE "SEMESTRE".
           05 LINE  04 COLUMN 61 VALUE "MENSALIDADE".

       01  SCR-SEPARADOR.
           05 LINE  05 COLUMN 02 VALUE "----".
           05 LINE  05 COLUMN 07 VALUE "--------------- ".
           05 LINE  05 COLUMN 31 VALUE "--------------- ".
           05 LINE  05 COLUMN 51 VALUE "---".
           05 LINE  05 COLUMN 61 VALUE "----- ".

       01  SCR-ALERTA.
           05 LINE  24 COLUMN 18 VALUE "PRESSIONE ENTER P/ SAIR"
                              BACKGROUND-COLOR 3.

       01  SCR-RODAPE.
           05 LINE  25 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

       01  SCR-MSGSAIR.
           05 LINE  23 COLUMN 60 VALUE "DIGITE SAIR PARA ENCERRAR".

       01  SCR-PAGINA.
           05 LINE  23 COLUMN 60 VALUE "PAGINA : ".
           05 LINE  23 COLUMN 69 FROM  WRK-PAGINA.

       01  SCR-DADOS-IMPRESSAO.
           05 LINE  05 COLUMN 30 VALUE "DESEJA GERAR O RELATORIO ?".
           05 LINE  06 COLUMN 30 VALUE "(S) PARA SIM, QUALQUER P/ SAIR".
           05 LINE  05 COLUMN 62 USING WRK-OPCAO.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------
           INITIALISE WRK-TEMP-DADOS, ARQ-DADOS, WRK-MSG,
                      WRK-QTD-ALUNOSCAD, WRK-FLAG-ENCONTRADO, WRK-APOIO.

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

      *--------------- AREA DE INSERCAO DE DADOS -----------------------
           DISPLAY SCR-MSGSAIR.
           ACCEPT SCR-DADOS.
           MOVE   FUNCTION UPPER-CASE(WRK-TEMP-CURSO)
                                    TO WRK-TEMP-CURSO.

           MOVE SPACES              TO SCR-MSGSAIR.
           DISPLAY SCR-MSGSAIR.

      *-------------------- AREA DE VERIFICACAO ------------------------
           IF WRK-TEMP-CURSO  EQUAL 'SAIR'
               PERFORM 0300-FINALIZAR
              GOBACK
           END-IF.

           IF WRK-TEMP-CURSO  EQUAL SPACES
               MOVE WRK-MSG-REG-INCOMPLETO    TO WRK-MSG
               PERFORM 0900-MOSTRA
               GO TO 0200-PROCESSAR
              GOBACK
           END-IF.

           DISPLAY SCR-CABECALHO
           DISPLAY SCR-SEPARADOR
           MOVE 6                             TO WRK-LINHA

      *-------------------- AREA DE LEITURA ----------------------------

           READ ALUNOS

           ADD 1                              TO WRK-PAGINA

           PERFORM UNTIL FS-ALUNOS  NOT EQUAL ZEROS

      *----------------- AREA PARA DETERMINAR A CONTAGEM ---------------
           IF WRK-TEMP-CURSO EQUAL ARQ-CURSO
              DISPLAY SCR-PAGINA
              MOVE 1                          TO WRK-FLAG-ENCONTRADO
              ADD  1                          TO WRK-QTD-ALUNOSCAD

              DISPLAY  ARQ-RM          LINE WRK-LINHA COLUMN 02
              DISPLAY  ARQ-NOMEALUNO   LINE WRK-LINHA COLUMN 07
              DISPLAY  ARQ-CURSO       LINE WRK-LINHA COLUMN 31
              DISPLAY  ARQ-SEMESTRE    LINE WRK-LINHA COLUMN 51
              DISPLAY  ARQ-MENSALIDADE LINE WRK-LINHA COLUMN 61

              ADD  1                          TO WRK-LINHA
              IF WRK-LINHA  GREATER 16
                 ADD  1                       TO WRK-PAGINA
                 MOVE 6                       TO WRK-LINHA
                 MOVE "PRESSIONE ENTER PARA AVANCAR"
                                              TO WRK-MSG
                 PERFORM 0900-MOSTRA
                 DISPLAY SCR-TELA
                 DISPLAY SCR-DADOS
                 DISPLAY SCR-SEPARADOR
              END-IF
           END-IF

           READ ALUNOS

           END-PERFORM.



           IF  WRK-FLAG-ENCONTRADO  EQUAL  1
               DISPLAY SCR-CABECALHO
               DISPLAY SCR-SEPARADOR
               DISPLAY SCR-RESULTDADOS
               DISPLAY SCR-ALERTA
               MOVE SPACES                    TO WRK-MSG
               PERFORM 0900-MOSTRA
           ELSE
               MOVE WRK-MSG-CURSO-NAOENCONT   TO WRK-MSG
               PERFORM 0900-MOSTRA
               DISPLAY SCR-ALERTA
               PERFORM 0300-FINALIZAR
               GOBACK
           END-IF.

      *------------------ AREA DE IMPRESSAO ----------------------------
           DISPLAY SCR-TELA

           ACCEPT SCR-DADOS-IMPRESSAO.

           IF WRK-OPCAO EQUAL "S" OR
              WRK-OPCAO EQUAL "s"

              CALL "PROGRELD" USING WRK-TEMP-CURSO WRK-FLAG-ENCONTRADO

              IF WRK-FLAG-ENCONTRADO EQUAL 1
                  MOVE WRK-REGISTRO      TO WRK-MSG
                  DISPLAY SCR-ALERTA
                  PERFORM 0900-MOSTRA
              ELSE
                  MOVE WRK-SEMCONFIRM    TO WRK-MSG
                  DISPLAY SCR-ALERTA
                  PERFORM 0900-MOSTRA
              END-IF

           ELSE
                  MOVE WRK-SEMCONFIRM    TO WRK-MSG
                  DISPLAY SCR-ALERTA
                  PERFORM 0900-MOSTRA
           END-IF.




      *-----------------------------------------------------------------
       0200-PROCESSAR-99-FIM                                    SECTION.
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
