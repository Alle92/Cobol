       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGRV01.
      *-----------------------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 06/04/2024
      *--- OBJETIVOS: LEITURA DE UM ARQUIVO SEQUENCIAL, ONDE PODE LER
      *----    VARIOS REGISTROS DENTRO DO ARQUIVO.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *---- DEFINICAO DE ARQUIVOS
      *----
      *---- ARQUIVO        TIPO I/O         BOOK
      *---- ALUNOS            E             BOKALU
      *----
      *-----------------------------------------------------------------

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
           05 WRK-TEMP-RM               PIC X(04).
           05 WRK-TEMP-NOMEALUNO        PIC X(20).
           05 WRK-TEMP-CURSO            PIC X(15).
           05 WRK-TEMP-SEMESTRE         PIC X(02).
           05 WRK-TEMP-MENSALIDADE      PIC 9(06).

      *------------- VARIAVEIS DE STATUS
       77  FS-ALUNOS                    PIC 9(02).

      *------------- VARIAVEIS DE MENSAGENS
       COPY "APOIO".

      *------------- VARIAVEL DE DECISAO -------------------------------
       77  WRK-CONFIRMAR                PIC X(01) VALUE SPACES.

      *-----------------------------------------------------------------

       01  WRK-APOIO.
           05 WRK-SEGURATELA                   PIC X(01) VALUE SPACES.
           05 WRK-OPCAO                        PIC X(01).
           05 WRK-VERIFICAR                    PIC 9(01) VALUE ZEROS.

      *-----------------------------------------------------------------
       SCREEN                                                  SECTION.
      *-----------------------------------------------------------------

       01  SCR-TELA.
           05 BLANK SCREEN.
           05 LINE  01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
           05 LINE  01 COLUMN 32 VALUE "CADASTRO DE ALUNO"
                  BACKGROUND-COLOR 3.

       01  SCR-DADOS.
           05 LINE  07 COLUMN 30 VALUE "RM...............".
           05 LINE  07 COLUMN 60 USING WRK-TEMP-RM.
           05 LINE  08 COLUMN 30 VALUE "NOME ALUNO.......".
           05 LINE  08 COLUMN 60 USING WRK-TEMP-NOMEALUNO.
           05 LINE  09 COLUMN 30 VALUE "CURSO............".
           05 LINE  09 COLUMN 60 USING WRK-TEMP-CURSO.
           05 LINE  10 COLUMN 30 VALUE "SEMESTRE.........".
           05 LINE  10 COLUMN 60 USING WRK-TEMP-SEMESTRE.
           05 LINE  11 COLUMN 30 VALUE "MENSALIDADE......".
           05 LINE  11 COLUMN 60 USING WRK-TEMP-MENSALIDADE.
           05 LINE  20 COLUMN 09 FROM  WRK-MSG.
           05 LINE  21 COLUMN 09 VALUE "APERTE TAB PARA CONTINUAR "
                       BACKGROUND-COLOR 3.
           05 LINE  21 COLUMN 35 VALUE "CADASTRANDO. "
                       BACKGROUND-COLOR 3.

       01  SCR-OPCAO.
           05 LINE  22 COLUMN 9  VALUE "DESEJA GRAVAR (S) OU (N) ? "
                              BACKGROUND-COLOR 2.
           05 LINE  22 COLUMN 38 USING WRK-OPCAO.
       01  SCR-RODAPE.
           05 LINE  23 COLUMN 18 FROM  WRK-MSG.
           05 LINE  23 COLUMN 45 USING WRK-SEGURATELA.

      *-----------------------------------------------------------------
       PROCEDURE                                               DIVISION.
      *-----------------------------------------------------------------
           INITIALIZE WRK-MSG WRK-TEMP-DADOS.
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
            OPEN EXTEND  ALUNOS.

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
           MOVE WRK-VAR-SAIDA                 TO WRK-MSG
           ACCEPT SCR-DADOS.

      *-------------------- AREA DE VERIFICACAO ------------------------
           IF WRK-TEMP-RM  EQUAL '9999'
               PERFORM 0300-FINALIZAR
              GOBACK
           END-IF.

      *----------------------AREA DE VERIFICACAO DE ESPACOS ------------

           IF WRK-TEMP-RM           EQUAL SPACES
              MOVE WRK-RM-INC                 TO WRK-MSG
              PERFORM 0900-MOSTRA
              GO  TO  0200-PROCESSAR
           END-IF.

           IF WRK-TEMP-NOMEALUNO    EQUAL SPACES
              MOVE WRK-NOME-INC               TO WRK-MSG
              PERFORM 0900-MOSTRA
              GO  TO  0200-PROCESSAR
           END-IF.

           IF WRK-TEMP-CURSO        EQUAL SPACES
              MOVE WRK-CURSO-INC              TO WRK-MSG
              PERFORM 0900-MOSTRA
              GO  TO  0200-PROCESSAR
           END-IF.

           IF WRK-TEMP-SEMESTRE     EQUAL SPACES
              MOVE WRK-SEM-INC                TO WRK-MSG
              PERFORM 0900-MOSTRA
              GO  TO  0200-PROCESSAR
           END-IF.

           IF WRK-TEMP-MENSALIDADE  EQUAL ZEROS
              MOVE WRK-MENS-INC               TO WRK-MSG
              PERFORM 0900-MOSTRA
              GO  TO  0200-PROCESSAR
           END-IF.

      *------------ VERIFICAR SE O RM ESTÁ VICIADO ---------------------

           IF WRK-TEMP-RM EQUAL "0000"
              MOVE WRK-MSG-REG-INCOMPLETO     TO WRK-MSG
              PERFORM 0900-MOSTRA

              GO TO 0200-PROCESSAR
           END-IF.

      *--------------- VERIFICAR SE O REGISTRO EXISTE ------------------

           CALL "PROGVERI" USING WRK-TEMP-RM, WRK-VERIFICAR.

           IF WRK-VERIFICAR EQUAL 1
              MOVE WRK-MSG-REG-JAEXISTE       TO WRK-MSG
              PERFORM 0900-MOSTRA
              PERFORM 0300-FINALIZAR
              GOBACK
           END-IF.

      *--------------- AREA DE CONFIRMACAO -----------------------------

           ACCEPT SCR-OPCAO.

      *--------------------- AREA DE GRAVACAO --------------------------
           IF WRK-OPCAO         EQUAL "S"  OR
              WRK-OPCAO         EQUAL "s"

                MOVE WRK-TEMP-RM              TO ARQ-RM
                MOVE WRK-TEMP-NOMEALUNO       TO ARQ-NOMEALUNO
                MOVE WRK-TEMP-CURSO           TO ARQ-CURSO
                MOVE WRK-TEMP-SEMESTRE        TO ARQ-SEMESTRE
                MOVE WRK-TEMP-MENSALIDADE     TO ARQ-MENSALIDADE

                WRITE ARQ-DADOS

                MOVE WRK-REGISTRO             TO WRK-MSG
           ELSE
                MOVE WRK-SEMCONFIRM           TO WRK-MSG
           END-IF.

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
