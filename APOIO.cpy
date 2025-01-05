       01  WRK-MSGS.
           05 WRK-MSG                   PIC X(40) VALUE SPACES.
           05 WRK-MSG-OPEN              PIC X(40)
                                 VALUE "ERRO NA ABERTURA DO ARQUIVO".
           05 WRK-MSG-OPEN2             PIC X(40)
                                 VALUE "ARQUIVO ABERTO COM SUCESSO".
           05 WRK-MSG-CLOSE             PIC X(40)
                                 VALUE "ERRO AO FECHAR O ARQUIVO".
           05 WRK-MSG-VAZIO             PIC X(40)
                                 VALUE "ARQUIVO VAZIO".
           05 WRK-SEMCONFIRM            PIC X(40)
                                 VALUE "RELATORIO NAO GERADO".
           05 WRK-REGISTRO              PIC X(40)
                                 VALUE "REGISTRO GRAVADOS".
           05 WRK-ERRO-VALOR            PIC X(40)
                                 VALUE "VALOR INCONSISTENTE".
           05 WRK-ERRO-CURSO            PIC X(40)
                                 VALUE "CURSO INEXISTENTE".
           05 WRK-MSG-OPCAO-ERRO        PIC X(40)
                                 VALUE "ENTRE COM A OPCAO CORRETA".
           05 WRK-MSG-OPCAO-AGRD        PIC X(40)
                                 VALUE "OBRIGADO E VOLTE SEMPRE".
           05 WRK-MSG-REG-INCOMPLETO    PIC X(40)
                                 VALUE "REGISTRO INCOMPLETO".
           05 WRK-MSG-REG-ENCONTRADO    PIC X(40)
                                 VALUE "REGISTRO ENCONTRADO".
           05 WRK-MSG-REG-NAOENCONT     PIC X(40)
                                 VALUE "REGISTRO NAO ENCONTRADO".
           05 WRK-MSG-REG-JAEXISTE      PIC X(40)
                                 VALUE "REGISTRO JA EXISTENTE".
           05 WRK-VAR-SAIDA             PIC X(40)
                                 VALUE "DIGITE 9999 PARA SAIR".
           05 WRK-RM-INC                PIC X(40)
                                 VALUE "RM INCOMPLETO".
           05 WRK-NOME-INC              PIC X(40)
                                 VALUE "NOME SEM REGISTRO".
           05 WRK-CURSO-INC             PIC X(40)
                                 VALUE "CURSO SEM REGISTRO".
           05 WRK-SEM-INC               PIC X(40)
                                 VALUE "SEMESTRE SEM REGISTRO".
           05 WRK-MENS-INC              PIC X(40)
                                 VALUE "MENSALIDADE SEM VALOR".
           05 WRK-MSG-CURSO-NAOENCONT   PIC X(40)
                                 VALUE "CURSO NAO ENCONTRADO".
