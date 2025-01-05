       IDENTIFICATION                                          DIVISION.
       PROGRAM-ID. PROGRELD.
      *-------------------------------------------------
      *---- SOBRE O MODULO
      *----
      *--- AUTOR    : ALEXANDRE
      *--- EMPRESA  : XPTO
      *--- DATA     : 19/04/2024
      *--- OBJETIVOS: GERAR O RELATORIO DOS ALUNOS DO CURSO DE
      *---             DISCIPLINA
      *---------------------------------------------------
      *---------------------------------------------------
      *---- DEFINICAO DE ARQUIVOS
      *----
      *---- ARQUIVO        TIPO I/O         BOOK
      *---- ALUNOS            O             BOKALU
      *---- DISCIPLINA        I              ---
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
*           SELECT ALUNOS  ASSIGN TO
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
       01  ARQ-RELDISCIPLINA                  PIC X(47).

      *-----------------------------------------------------------------
       WORKING-STORAGE                                         SECTION.
      *-----------------------------------------------------------------

      *------------- VARIAVEIS DE STATUS
       77  FS-ALUNOS                          PIC 9(02).
       77  FS-DISCIPLINA                      PIC 9(02).

      *-----------------------------------------------------------------
       LINKAGE                                                  SECTION.
      *-----------------------------------------------------------------

       01  LNK-TEMP-CURSO                     PIC X(15).
       01  LNK-FLAG-ENCONTRADO                PIC 9(01).


      *-----------------------------------------------------------------
       PROCEDURE                                                DIVISION
       USING LNK-TEMP-CURSO, LNK-FLAG-ENCONTRADO.
      *-----------------------------------------------------------------

       0001-PRINCIPAL                          SECTION.

            PERFORM 0100-INICIALIZAR.
            PERFORM 0200-PROCESSAR.
            PERFORM 0300-FINALIZAR.
            GOBACK.

      *-----------------------------------------------------------------
       0100-INICIALIZAR                                        SECTION.
      *-----------------------------------------------------------------

      *----------------- AREA DE ABERTURA DE ARQUIVOS ------------------
           OPEN INPUT ALUNOS.

           IF  FS-ALUNOS NOT EQUAL ZEROS
               CLOSE ALUNOS
               GOBACK
           END-IF.

      *----------------- AREA DE ABERTURA DE ARQUIVOS ------------------
           OPEN OUTPUT DISCIPLINA.

           IF  FS-DISCIPLINA NOT EQUAL ZEROS
               CLOSE DISCIPLINA
               GOBACK
           END-IF.

      *-----------------------------------------------------------------
       0100-INICIALIZAR-99-FIM                                 SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0200-PROCESSAR                                          SECTION.
      *-----------------------------------------------------------------

      *-------------------- AREA DE LEITURA ----------------------------

           READ ALUNOS.


           PERFORM UNTIL FS-ALUNOS NOT EQUAL ZEROS

              IF LNK-TEMP-CURSO    EQUAL ARQ-CURSO

                  MOVE  1                     TO LNK-FLAG-ENCONTRADO
                  MOVE ARQ-DADOS              TO ARQ-RELDISCIPLINA
                  WRITE ARQ-RELDISCIPLINA

              END-IF

           READ ALUNOS

           END-PERFORM.

           IF  LNK-FLAG-ENCONTRADO EQUAL 1
               PERFORM 0300-FINALIZAR
           END-IF.

      *-----------------------------------------------------------------
       0200-PROCESSAR-99-FIM                                   SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       0300-FINALIZAR                                          SECTION.
      *-----------------------------------------------------------------

      *--------------------- AREA DE FECHAMENTO DE ARQUIVOS ------------

           CLOSE ALUNOS.

           CLOSE DISCIPLINA.

      *-----------------------------------------------------------------
       0300-FINALIZAR-99-FIM                                   SECTION.
      *-----------------------------------------------------------------
