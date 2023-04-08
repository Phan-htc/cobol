      *--------------------------------------------------------*
      * DESCRIPTION DE L'ENREGISTREMENT DU FICHIER COMPTE FCPTE*
      * EN ENTREE DU PROGRAMME                                 *
      *--------------------------------------------------------*
      *
       01  WS-ENRG-F-CPTE.
           05 WS-CPTE-CPTE                     PIC 9(10).
           05 WS-CPTE-CPTE-X REDEFINES WS-CPTE-CPTE
                                               PIC X(10).
              88 WS-CPTE-CPTE-MAX              VALUE HIGH-VALUES.
      *
           05 WS-CPTE-DCREA.
               10 WS-CPTE-DCREA-SSAA.
                  15  WS-CPTE-DCREA-SS         PIC 9(2).
                  15  WS-CPTE-DCREA-AA         PIC 9(2).
               10 WS-CPTE-DCREA-MM             PIC 9(2).
               10 WS-CPTE-DCREA-JJ             PIC 9(2).
      *
           05 WS-CPTE-SOLDE                    PIC S9(8)V99
                                               VALUE ZERO.
      *
           05 WS-CPTE-DMAJ.
               10 WS-CPTE-DMAJ-SSAA.
                  15 WS-CPTE-DMAJ-SS           PIC 9(2).
                  15 WS-CPTE-DMAJ-AA           PIC 9(2).
               10 WS-CPTE-DMAJ-MM              PIC 9(2).
               10 WS-CPTE-DMAJ-JJ              PIC 9(2).
