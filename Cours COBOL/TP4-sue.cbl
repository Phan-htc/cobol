      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO44B                                   *
      *  NOM DU REDACTEUR : SERVETO                                   *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 23/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *    TRAITEMENT DE MOUVEMENTS BANCAIRES ET DE COMPTES CLIENTS,  *
      *    à METTRE A JOUR, à CRéER ET CLOTURER.                      *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      *               !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO44B.
      *
      *                  ==============================               *
      *=================<  ENVIRONMENT      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
      *
      *======================
       CONFIGURATION SECTION.
      *======================
      *
      *--------------
       SPECIAL-NAMES.
      *--------------
           DECIMAL-POINT IS COMMA.
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
      *
      *                      -------------------------------------------
      *                      XXXXXXX : FICHIER DES XXXXX
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-F-MVTS-E.
      *
           SELECT  F-CPTE-ES           ASSIGN TO IO001
                   ORGANIZATION        IS INDEXED
                   ACCESS MODE         IS RANDOM
                   RECORD KEY          IS FS-CPT-CPTE
                   FILE STATUS         IS WS-FS-F-CPTE-ES.
      *
           SELECT  F-ETATCLI-S         ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-F-ETATCLI-S.
      *
           SELECT  F-ETATANO-S         ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-F-ETATANO-S.
      *                      -------------------------------------------
      *
      *
      *                  ==============================               *
      *=================<       DATA        DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *=============
       FILE SECTION.
      *=============
      *
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENR-F-MVTS            PIC X(50).
      *
       FD  F-CPTE-ES
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENR-F-CPTE.
           05  FS-CPT-CPTE          PIC X(10).
           05  FILLER               PIC X(40).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENR-F-ETATCLI         PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENR-F-ETATANO         PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *------------------------
      *VARIABLES FILE STATUS
      *------------------------
       77  WS-FS-F-MVTS-E           PIC X(2).
           88  READ-MVTS-OK             VALUE '00'
                                              '10'.
           88  MVTS-EMPTY               VALUE '10'.
           88  MVTS-OK                  VALUE '00'.
      *
       77  WS-FS-F-CPTE-ES          PIC X(2).
           88  READ-CPTE-OK             VALUE '00'
                                              '23'.
           88  CPTE-EMPTY               VALUE '23'.
           88  CPTE-OK                  VALUE '00'.
      *
       77  WS-FS-F-ETATCLI-S        PIC X(2).
           88  ETATCLI-OK               VALUE '00'.
      *
       77  WS-FS-F-ETATANO-S        PIC X(2).
           88  ETATANO-OK               VALUE '00'.
      *
       77  WS-BUFFER                PIC X(80).
      *
      *-----------------------
      *VARIABLES FICHIERS
      *-----------------------
      *
           COPY TP4LEDIT.
           COPY TP4MVTS.
           COPY TP4CPT.
      *
      *-----------------------
      *VARIABLES COMPTEUR
      *-----------------------
      *
       77  WS-CCLI                  PIC S9(4)     COMP.
       77  WS-CCLINEW               PIC S9(4)     COMP VALUE ZERO.
       77  WS-CCLIST                PIC S9(4)     COMP VALUE ZERO.
       77  WS-CCLICLOT              PIC S9(4)     COMP VALUE ZERO.
      *
       77  WS-CMVT                  PIC S9(4)     COMP.
       77  WS-CDEP                  PIC S9(4)     COMP VALUE ZERO.
       77  WS-CERR                  PIC S9(4)     COMP VALUE ZERO.
           88  NO-ERR                                  VALUE ZERO.
       77  WS-CRET                  PIC S9(4)     COMP VALUE ZERO.
       77  WS-CCB                   PIC S9(4)     COMP VALUE ZERO.
       77  ws-CCLO                  PIC S9(4)     COMP VALUE ZERO.
      *
       77  WS-TOT-MTDB              PIC S9(11)V99 COMP-3.
           88  MTDB-NUL                                VALUE 0.
       77  WS-TOT-MTCR              PIC S9(11)V99 COMP-3.
           88  MTCR-NUL                                VALUE 0.
       77  WS-TOT-MT-ERR            PIC S9(11)V99 COMP-3
                                                       VALUE ZERO.
       77  WS-SOLDE-CALC            PIC S9(11)V99 COMP-3.
       77  WS-SOLDE-TEMP            PIC S9(11)V99 COMP-3.
      *
       77  WS-LINE                  PIC S9(4)     COMP.
           88 LINE-0                                   VALUE 0.
           88 LINE-5                                   VALUE 5.
      *
       77  WS-PAGE                  PIC S9(4)     COMP.
      *
       77  WS-CLOSE                 PIC 9.
           88  CLOSE-CPT                               VALUE 1.
           88  KEEP-CPT                                VALUE 0.
      *
      *-----------------------
      *VARIABLES DATE
      *-----------------------
      *
       01 WS-DATE.
          05 WS-DATE-ANNEE.
             10 WS-DATE-SS          PIC 99.
             10 WS-DATE-AA          PIC 99.
          05 WS-DATE-MM             PIC 99.
          05 WS-DATE-JJ             PIC 99.
      *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION.
      *
      *===============================================================*
      *
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
      *DEBUT TRT PRINCIPAL.
      *
       0000-TRT-PRINCIPAL-DEB.
      *
      *OREILLETTE GAUCHE
      *
            PERFORM  6000-OPEN-CPTE-DEB
               THRU  6000-OPEN-CPTE-FIN.
      *
            PERFORM  6010-OPEN-MVTS-DEB
               THRU  6010-OPEN-MVTS-FIN.
      *
            PERFORM  6020-OPEN-ETATCLI-DEB
               THRU  6020-OPEN-ETATCLI-FIN.
      *
            PERFORM  6030-OPEN-ETATANO-DEB
               THRU  6030-OPEN-ETATANO-FIN.
      *
            PERFORM  6040-READ-MVTS-DEB
               THRU  6040-READ-MVTS-FIN.
      *
            IF MVTS-EMPTY
              DISPLAY  'FICHIER F-MVTS-E VIDE.'
      *
            END-IF.
      *
            PERFORM  7000-INIT-DATE-DEB
               THRU  7000-INIT-DATE-FIN.
      *
            PERFORM  8000-PGARDE-ETATCLI-DEB
               THRU  8000-PGARDE-ETATCLI-FIN.
      *
            PERFORM  8010-PGARDE-ETATANO-DEB
               THRU  8010-PGARDE-ETATANO-FIN.
      *
      *ITERATIVE
      *
            PERFORM  1000-TRT-CPT-DEB
               THRU  1000-TRT-CPT-FIN
              UNTIL  MVTS-EMPTY.
      *
      *OREILLETTE DROITE
      *
            PERFORM  7010-CALC-NB-CLIENT-MVT-DEB
               THRU  7010-CALC-NB-CLIENT-MVT-FIN.
      *
            IF NO-ERR
              PERFORM 8020-DET-ANO-OK-DEB
                 THRU 8020-DET-ANO-OK-FIN
      *
            ELSE
      *
              PERFORM 8030-FIN-ETATANO-DEB
                 THRU 8030-FIN-ETATANO-FIN
      *
            END-IF.
      *
            PERFORM  6050-CLOSE-MVTS-DEB
               THRU  6050-CLOSE-MVTS-FIN.
      *
            PERFORM  6060-CLOSE-CPTE-DEB
               THRU  6060-CLOSE-CPTE-FIN.
      *
            PERFORM  6070-CLOSE-ETATCLI-DEB
               THRU  6070-CLOSE-ETATCLI-FIN.
      *
            PERFORM  6080-CLOSE-ETATANO-DEB
               THRU  6080-CLOSE-ETATANO-FIN.
      *
            PERFORM  8999-STATISTIQUES-DEB
               THRU  8999-STATISTIQUES-FIN.
      *
            PERFORM  9999-FIN-PROGRAMME-DEB
               THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-TRT-PRINCIPAL-FIN.
            STOP RUN.
      *
      *FIN TRT PRINCIPAL.
      *
      *------------------
      *TRT COMPTE
      *------------------
       1000-TRT-CPT-DEB.
      *
      *OREILLETTES Gauche.
      *
      *
            PERFORM  7020-PREP-CPT-KEY-DEB
               THRU  7020-PREP-CPT-KEY-FIN.
      *
            PERFORM  6090-READ-CPTE-DEB
               THRU  6090-READ-CPTE-FIN.
      *
      *ALTERNATIVE SIMPLE
      *
            IF CPTE-OK
               PERFORM  2000-CPT-EXIST-DEB
                  THRU  2000-CPT-EXIST-FIN
            ELSE
               PERFORM  2010-NO-CPT-DEB
                  THRU  2010-NO-CPT-FIN
            END-IF.
      *
       1000-TRT-CPT-FIN.
            EXIT.
      *-----------------
      *FIN TRT COMPTE
      *-----------------
      *
      *DEBUT TRT CPT
      *
      ******************
      *COMPTE EXISTE
      ******************
       2000-CPT-EXIST-DEB.
      *
      *OREILLETTE GAUCHE
      *
            PERFORM  7030-PREP-CPT-EXIST-DEB
               THRU  7030-PREP-CPT-EXIST-FIN.
      *
      * ITERATIVE
      *
            PERFORM  3000-MVT-CPT-EXIST-DEB
               THRU  3000-MVT-CPT-EXIST-FIN
              UNTIL  MVTS-EMPTY OR
                     WS-CPTS-CPT NOT = WS-MVTS-CPTE.
      *
      *OREILLETTE DROITE
      *
            IF CLOSE-CPT
      *
               PERFORM  7050-PREP-CLOT-CPT-DEB
                  THRU  7050-PREP-CLOT-CPT-FIN
      *
               PERFORM  7160-INCR-CLICLOT-DEB
                  THRU  7160-INCR-CLICLOT-FIN
      *
               PERFORM  6100-DEL-CPT-DEB
                  THRU  6100-DEL-CPT-FIN
      *
            ELSE
      *
               PERFORM  7170-INCR-CLIST-DEB
                  THRU  7170-INCR-CLIST-FIN
      *
               PERFORM  6110-REWRITE-CPT-DEB
                  THRU  6110-REWRITE-CPT-FIN
      *
            END-IF.
      *
            IF NOT (MTDB-NUL AND MTCR-NUL)
      *
               PERFORM  7040-PREP-FIN-ETATCLI-DEB
                  THRU  7040-PREP-FIN-ETATCLI-FIN
      *
               PERFORM  8040-IMP-FIN-CPT-DEB
                  THRU  8040-IMP-FIN-CPT-FIN
      *
            END-IF.
      *
       2000-CPT-EXIST-FIN.
            EXIT.
      ******************
      *PAS DE COMPTE
      ******************
       2010-NO-CPT-DEB.
      *
      *OREILLETTE GAUCHE
      *
            PERFORM  7060-INIT-TRT-DEB
               THRU  7060-INIT-TRT-FIN.
      *
      *ITERATIVE
      *
            PERFORM  3010-MVT-NO-CPT-DEB
               THRU  3010-MVT-NO-CPT-FIN
              UNTIL  MVTS-EMPTY OR
                     WS-CPTS-CPT NOT = WS-MVTS-CPTE.
      *
      *OREILLETTE DROITE
      *
            IF NOT (MTDB-NUL AND MTCR-NUL)
      *
               PERFORM  7070-CCLINEW-DEB
                  THRU  7070-CCLINEW-FIN
      *
               PERFORM  7040-PREP-FIN-ETATCLI-DEB
                  THRU  7040-PREP-FIN-ETATCLI-FIN
      *
               IF NOT CLOSE-CPT
      *
                 PERFORM  6160-WRITE-CPTES-DEB
                    THRU  6160-WRITE-CPTES-FIN
      *
               ELSE
      *
                 PERFORM  7050-PREP-CLOT-CPT-DEB
                    THRU  7050-PREP-CLOT-CPT-FIN
      *
                 PERFORM  7160-INCR-CLICLOT-DEB
                    THRU  7160-INCR-CLICLOT-FIN
      *
               END-IF
      *
               PERFORM  8040-IMP-FIN-CPT-DEB
                  THRU  8040-IMP-FIN-CPT-FIN
      *
            END-IF.
      *
       2010-NO-CPT-FIN.
            EXIT.
      ******************
      *FIN TRT CPT
      *-----------------
      *
      *DEBUT TRT MVT AC CPT
      ******************
       3000-MVT-CPT-EXIST-DEB.
      *
      *OREILLETTE GAUCHE
      *
            IF RETRAIT OR CB OR DEPOT
      *
              IF LINE-5
      *
                PERFORM  7090-PREP-INTER-CPT-DEB
                   THRU  7090-PREP-INTER-CPT-FIN
      *
                PERFORM  8040-IMP-FIN-CPT-DEB
                   THRU  8040-IMP-FIN-CPT-FIN
      *
              END-IF
      *
              IF LINE-0
      *
                PERFORM  7080-PREP-ENT-CLI-DEB
                   THRU  7080-PREP-ENT-CLI-FIN
      *
                PERFORM  8050-IMP-ENT-CLI-DEB
                   THRU  8050-IMP-ENT-CLI-FIN
      *
              END-IF
      *
            END-IF.
      *
      *ALTERNATIVE MULTIPLE
      *
            EVALUATE TRUE
      *
               WHEN CLOTURE  PERFORM 4000-TRT-CLOTURE-DEB
                                THRU 4000-TRT-CLOTURE-FIN
      *
               WHEN RETRAIT  PERFORM 4010-TRT-RETRAIT-DEB
                                THRU 4010-TRT-RETRAIT-FIN
      *
               WHEN CB       PERFORM 4020-TRT-CB-DEB
                                THRU 4020-TRT-CB-FIN
      *
               WHEN DEPOT    PERFORM 4030-TRT-DEP-DEB
                                THRU 4030-TRT-DEP-FIN
      *
               WHEN OTHER    PERFORM 4040-TRT-ANO-DEB
                                THRU 4040-TRT-ANO-FIN
            END-EVALUATE.
      *
      *OREILLETTE DROITE
      *
            IF (RETRAIT OR CB OR DEPOT)
      *
               PERFORM  8060-IMP-DET-CLI-DEB
                  THRU  8060-IMP-DET-CLI-FIN
      *
            END-IF.
      *
            PERFORM  6040-READ-MVTS-DEB
               THRU  6040-READ-MVTS-FIN.
      *
       3000-MVT-CPT-EXIST-FIN.
            EXIT.
      ******************
      *FIN TRT MVT AC CPT
      *-----------------
      *
      *DEBUT TRT MVT NO CPT
      *
      ******************
       3010-MVT-NO-CPT-DEB.
      *
            IF RETRAIT OR CB OR DEPOT
      *
              IF LINE-5
      *
                 PERFORM  7090-PREP-INTER-CPT-DEB
                    THRU  7090-PREP-INTER-CPT-FIN
      *
                 PERFORM  8040-IMP-FIN-CPT-DEB
                    THRU  8040-IMP-FIN-CPT-FIN
      *
              END-IF
      *
              IF LINE-0
      *
                 PERFORM  7080-PREP-ENT-CLI-DEB
                    THRU  7080-PREP-ENT-CLI-FIN
      *
                 PERFORM  8050-imp-ent-cli-DEB
                    THRU  8050-imp-ent-cli-FIN
      *
               END-IF
      *
            END-IF.

      *
            PERFORM  7180-INIT-DCREA-DEB
               THRU  7180-INIT-DCREA-FIN
      *
      *ALTERNATIVE MULTIPLE
      *
            EVALUATE TRUE
      *
               WHEN CLOTURE  PERFORM 4000-TRT-CLOTURE-DEB
                                THRU 4000-TRT-CLOTURE-FIN
      *
               WHEN RETRAIT  PERFORM 4010-TRT-RETRAIT-DEB
                                THRU 4010-TRT-RETRAIT-FIN
      *
               WHEN CB       PERFORM 4020-TRT-CB-DEB
                                THRU 4020-TRT-CB-FIN
      *
               WHEN DEPOT    PERFORM 4030-TRT-DEP-DEB
                                THRU 4030-TRT-DEP-FIN
      *
               WHEN OTHER    PERFORM 4040-TRT-ANO-DEB
                                THRU 4040-TRT-ANO-FIN
            END-EVALUATE.
      *
      *OREILLETTE DROITE
      *
            IF (RETRAIT OR CB OR DEPOT)
               PERFORM  8060-IMP-DET-CLI-DEB
                  THRU  8060-IMP-DET-CLI-FIN
            END-IF.
      *
            PERFORM  6040-READ-MVTS-DEB
               THRU  6040-READ-MVTS-FIN.
      *
       3010-MVT-NO-CPT-FIN.
            EXIT.
      ******************
      *FIN TRT MVT NO CPT
      *-----------------
      *
      *DEBUT TRT CODE MVT
      *
      ******************
       4000-TRT-CLOTURE-DEB.
      *
            PERFORM 7110-CALC-CLOT-DEB
               THRU 7110-CALC-CLOT-FIN.
      *
       4000-TRT-CLOTURE-FIN.
            EXIT.
      *
       4010-TRT-RETRAIT-DEB.
      *
            PERFORM 7120-CALC-DB-RET-DEB
               THRU 7120-CALC-DB-RET-FIN.
      *
       4010-TRT-RETRAIT-FIN.
            EXIT.
      *
       4020-TRT-CB-DEB.
      *
            PERFORM 7130-CALC-DB-CB-DEB
               THRU 7130-CALC-DB-CB-FIN.
      *
       4020-TRT-CB-FIN.
            EXIT.
      *
       4030-TRT-DEP-DEB.
      *
            PERFORM 7140-CALC-CR-DEP-DEB
               THRU 7140-CALC-CR-DEP-FIN.
      *
       4030-TRT-DEP-FIN.
            EXIT.
      *
       4040-TRT-ANO-DEB.
      *
            IF NO-ERR
               PERFORM 8080-IMP-ENT-ANO-DEB
                  THRU 8080-IMP-ENT-ANO-FIN
            END-IF.
      *
            PERFORM  7150-CALC-ERR-DEB
               THRU  7150-CALC-ERR-FIN.
      *
            PERFORM  8070-IMP-DET-ANO-DEB
               THRU  8070-IMP-DET-ANO-FIN.
      *
       4040-TRT-ANO-FIN.
            EXIT.
      *===============================================================*
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *
       6000-OPEN-CPTE-DEB.
            OPEN I-O F-CPTE-ES.
            IF NOT CPTE-OK
               DISPLAY 'ERREUR OUVERTURE DE F-CPTE-ES.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6000-OPEN-CPTE-FIN.
            EXIT.
      *
       6010-OPEN-MVTS-DEB.
            OPEN INPUT F-MVTS-E.
            IF NOT MVTS-OK
               DISPLAY 'ERREUR OUVERTURE DE F-MVTS-E'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6010-OPEN-MVTS-FIN.
            EXIT.
      *
       6020-OPEN-ETATCLI-DEB.
            OPEN OUTPUT F-ETATCLI-S.
            IF NOT ETATCLI-OK
               DISPLAY 'ERREUR OUVERTURE DE F-ETATCLI-S.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6020-OPEN-ETATCLI-FIN.
            EXIT.
      *
       6030-OPEN-ETATANO-DEB.
            OPEN OUTPUT F-ETATANO-S.
            IF NOT ETATANO-OK
               DISPLAY 'ERREUR OUVERTURE DE F-ETATANO-S.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-ETATANO-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6030-OPEN-ETATANO-FIN.
            EXIT.
      *
       6040-READ-MVTS-DEB.
            READ F-MVTS-E               INTO WS-ENRG-F-MVTS.
            IF NOT READ-MVTS-OK
               DISPLAY 'ERREUR LECTURE F-MVTS-E.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
      *
       6040-READ-MVTS-FIN.
            EXIT.
      *
       6050-CLOSE-MVTS-DEB.
            CLOSE F-MVTS-E.
            IF NOT MVTS-OK
               DISPLAY 'ERREUR FERMETURE DE F-MVTS-E.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6050-CLOSE-MVTS-FIN.
            EXIT.
      *
       6060-CLOSE-CPTE-DEB.
            CLOSE F-CPTE-ES.
            IF NOT CPTE-OK
               DISPLAY 'ERREUR FERMETURE DE F-CPTE-E.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6060-CLOSE-CPTE-FIN.
            EXIT.
      *
       6070-CLOSE-ETATCLI-DEB.
            CLOSE F-ETATCLI-S.
            IF NOT ETATCLI-OK
               DISPLAY 'ERREUR FERMETURE DE F-ETATCLI-S.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6070-CLOSE-ETATCLI-FIN.
            EXIT.
      *
       6080-CLOSE-ETATANO-DEB.
            CLOSE F-ETATANO-S.
            IF NOT ETATANO-OK
               DISPLAY 'ERREUR FERMETURE DE F-ETATANO-S.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-ETATANO-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6080-CLOSE-ETATANO-FIN.
            EXIT.
      *
       6090-READ-CPTE-DEB.
            READ F-CPTE-ES              INTO WS-ENRG-F-CPTS.
            IF NOT (CPTE-OK OR CPTE-EMPTY)
               DISPLAY 'ERREUR LECTURE DE F-CPTE-E.'
               DISPLAY 'VALEUR DU FILE STATUS : ' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6090-READ-CPTE-FIN.
            EXIT.
      *
       6100-DEL-CPT-DEB.
            DELETE F-CPTE-ES.
            IF NOT CPTE-OK
               DISPLAY 'PROBLEME DE SUPPRESSION DU FICHIER F-CPTE-ES'
               DISPLAY 'CODE:' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6100-DEL-CPT-FIN.
            EXIT.
      *
       6110-REWRITE-CPT-DEB.
            REWRITE FS-ENR-F-CPTE       FROM WS-ENRG-F-CPTS.
            IF NOT CPTE-OK
               DISPLAY 'PROBLEME DE REECRITURE DU FICHIER F-CPTE-ES'
               DISPLAY 'CODE:' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6110-REWRITE-CPT-FIN.
            EXIT.
      *
       6120-WRITE-FIRST-LINE-CLI-DEB.
            WRITE FS-ENR-F-ETATCLI      FROM WS-BUFFER AFTER PAGE.
            IF NOT ETATCLI-OK
               DISPLAY 'ERREUR ECRITURE F-ETATCLI-S'
               DISPLAY 'CODE:' WS-FS-F-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6120-WRITE-FIRST-LINE-CLI-FIN.
            EXIT.
      *
       6130-WRITE-DET-CLI-DEB.
            WRITE FS-ENR-F-ETATCLI      FROM WS-BUFFER.
            IF NOT ETATCLI-OK
               DISPLAY 'ERREUR ECRITURE F-ETATCLI-S'
               DISPLAY 'CODE:' WS-FS-F-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6130-WRITE-DET-CLI-FIN.
            EXIT.
      *
       6140-WRITE-FIRST-LINE-ANO-DEB.
            WRITE FS-ENR-F-ETATANO      FROM WS-BUFFER AFTER PAGE.
            IF NOT ETATANO-OK
               DISPLAY 'ERREUR ECRITURE F-ETATANO-S'
               DISPLAY 'CODE:' WS-FS-F-ETATANO-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6140-WRITE-FIRST-LINE-ANO-FIN.
            EXIT.
      *
       6150-WRITE-DET-ANO-DEB.
            WRITE FS-ENR-F-ETATANO      FROM WS-BUFFER.
            IF NOT ETATANO-OK
               DISPLAY 'ERREUR ECRITURE F-ETATANO-S'
               DISPLAY 'CODE:' WS-FS-F-ETATANO-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6150-WRITE-DET-ANO-FIN.
            EXIT.
      *
       6160-WRITE-CPTES-DEB.
            WRITE FS-ENR-F-CPTE         FROM WS-ENRG-F-CPTS.
            IF NOT CPTE-OK
               DISPLAY 'ERREUR ECRITURE F-CPTE-ES'
               DISPLAY 'CODE:' WS-FS-F-CPTE-ES
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6160-WRITE-CPTES-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-DATE-DEB.
            ACCEPT WS-DATE              FROM DATE YYYYMMDD.
      *
            MOVE WS-DATE-AA             TO WS-L7-AA-ED.
            MOVE WS-DATE-SS             TO WS-L7-SS-ED.
            MOVE WS-DATE-JJ             TO WS-L7-JJ-ED.
            MOVE WS-DATE-MM             TO WS-L7-MM-ED.
      *
            MOVE WS-DATE-ANNEE          TO WS-LETAT-SSAA-ED.
            MOVE WS-DATE-JJ             TO WS-LETAT-JJ-ED.
            MOVE WS-DATE-MM             TO WS-LETAT-MM-ED.
      *
            MOVE 1                      TO WS-LETAT-PAGE-ED.
       7000-INIT-DATE-FIN.
            EXIT.
      *
       7010-CALC-NB-CLIENT-MVT-DEB.
            ADD  WS-CERR WS-CRET WS-CCB WS-CDEP WS-CCLO
                                        GIVING WS-CMVT.
            ADD  WS-CCLINEW WS-CCLIST WS-CCLICLOT
                                        GIVING WS-CCLI.
       7010-CALC-NB-CLIENT-MVT-FIN.
            EXIT.
      *
       7020-PREP-CPT-KEY-DEB.
            MOVE WS-MVTS-CPTE
                                        TO FS-CPT-CPTE.
            SET  KEEP-CPT               TO TRUE.
            MOVE 1                      TO WS-PAGE.
            MOVE 0                      TO WS-LINE.
       7020-PREP-CPT-KEY-FIN.
            EXIT.
      *
       7030-PREP-CPT-EXIST-DEB.
            MOVE 0                      TO WS-TOT-MTDB.
            MOVE 0                      TO WS-TOT-MTCR.
            MOVE WS-DATE
                                        TO WS-CPTS-DMAJ.
            MOVE WS-CPTS-SOLDE
                                        TO WS-SOLDE-CALC.
            MOVE WS-MVTS-CPTE           TO WS-LETAT-NUMCPT-ED.
            MOVE SPACES                 TO WS-LETAT-OPEN-ED.
       7030-PREP-CPT-EXIST-FIN.
            EXIT.
      *
       7040-PREP-FIN-ETATCLI-DEB.
            MOVE WS-TOT-MTDB
                                        TO WS-LETAT-TOTDB-ED.
            MOVE WS-TOT-MTCR
                                        TO WS-LETAT-TOTCR-ED.
            MOVE 'NOUVEAU SOLDE'
                                        TO WS-LETAT-LIB-ED.
            MOVE WS-SOLDE-CALC          TO WS-LETAT-SOLD-ED.
            MOVE 'TOTAL DES OPERATIONS'
                                        TO WS-LETAT-TOT-LIB-ED.

       7040-PREP-FIN-ETATCLI-FIN.
            EXIT.
      *
       7050-PREP-CLOT-CPT-DEB.
            MOVE 'CLOTURE DE COMPTE'
                                        TO WS-LETAT-CLOSE-ED.
       7050-PREP-CLOT-CPT-FIN.
            EXIT.
      *
       7060-INIT-TRT-DEB.
            MOVE 0                      TO WS-TOT-MTDB
                                           WS-TOT-MTCR
                                           WS-SOLDE-CALC.
            MOVE WS-DATE                TO WS-CPTS-DMAJ.
            MOVE WS-MVTS-CPTE           TO WS-CPTS-CPT.
            MOVE 'CREATION DE COMPTE'   TO WS-LETAT-OPEN-ED.
       7060-INIT-TRT-FIN.
            EXIT.
      *
       7070-CCLINEW-DEB.
            COMPUTE WS-SOLDE-CALC = WS-SOLDE-CALC - WS-TOT-MTDB
                   + WS-TOT-MTCR.
            MOVE WS-SOLDE-CALC
                                        TO WS-CPTS-SOLDE.
            ADD  1                      TO WS-CCLINEW.
       7070-CCLINEW-FIN.
            EXIT.
      *
       7080-PREP-ENT-CLI-DEB.
            MOVE WS-PAGE                TO WS-LETAT-PAGE-ED.
            MOVE WS-CPTS-CPT
                                        TO WS-LETAT-NUMCPT-ED.
            MOVE 'ANCIEN SOLDE'
                                        TO WS-LETAT-LIB-ED.
            MOVE WS-SOLDE-CALC
                                        TO WS-LETAT-SOLD-ED.
            MOVE SPACES
                                        TO WS-LETAT-CLOSE-ED.
       7080-PREP-ENT-CLI-FIN.
            EXIT.
      *
       7090-PREP-INTER-CPT-DEB.
            MOVE WS-TOT-MTDB
                                        TO WS-LETAT-TOTDB-ED.
            MOVE WS-TOT-MTCR
                                        TO WS-LETAT-TOTCR-ED.
            MOVE 'SOUS TOTAL DES OPERATIONS'
                                        TO WS-LETAT-TOT-LIB-ED.
            MOVE 'SOLDE INTERMEDIAIRE'
                                        TO WS-LETAT-LIB-ED.
            COMPUTE WS-SOLDE-TEMP = WS-SOLDE-CALC - WS-TOT-MTDB
                   + WS-TOT-MTCR.
            MOVE WS-SOLDE-TEMP          TO WS-LETAT-SOLD-ED.
            ADD  1                      TO WS-PAGE.
            MOVE 0                      TO WS-LINE.
       7090-PREP-INTER-CPT-FIN.
            EXIT.
      *
       7110-CALC-CLOT-DEB.
            SET  CLOSE-CPT              TO TRUE.
            ADD  1                      TO WS-CCLO.
       7110-CALC-CLOT-FIN.
            EXIT.
      *
       7120-CALC-DB-RET-DEB.
            ADD  1                      TO WS-CRET.
            ADD  WS-MVTS-MT
                                        TO WS-TOT-MTDB.
            MOVE 'RETRAIT DAB'
                                        TO WS-LETAT-OP-LIB-ED.
            MOVE WS-MVTS-JJ
                                        TO WS-LETAT-OP-JJ-ED.
            MOVE WS-MVTS-MM
                                        TO WS-LETAT-OP-MM-ED.
            MOVE WS-MVTS-AA
                                        TO WS-LETAT-OP-AA-ED.
            MOVE WS-MVTS-SS
                                        TO WS-LETAT-OP-SS-ED.
            MOVE WS-MVTS-MT
                                        TO WS-LETAT-OP-DEBIT-ED.
            MOVE 0                      TO WS-LETAT-OP-CREDIT-ED.
            MOVE SPACES                 TO WS-LETAT-OPEN-ED.
            ADD  1                      TO WS-LINE.
       7120-CALC-DB-RET-FIN.
            EXIT.
      *
       7130-CALC-DB-CB-DEB.
            ADD  1                      TO WS-LINE.
            ADD  1                      TO WS-CCB.
            ADD  WS-MVTS-MT
                                        TO WS-TOT-MTDB.
            MOVE 'CARTE BLEUE'
                                        TO WS-LETAT-OP-LIB-ED.
            MOVE WS-MVTS-JJ
                                        TO WS-LETAT-OP-JJ-ED.
            MOVE WS-MVTS-MM
                                        TO WS-LETAT-OP-MM-ED.
            MOVE WS-MVTS-AA
                                        TO WS-LETAT-OP-AA-ED.
            MOVE WS-MVTS-SS
                                        TO WS-LETAT-OP-SS-ED.
            MOVE WS-MVTS-MT
                                        TO WS-LETAT-OP-DEBIT-ED.
            MOVE 0                      TO WS-LETAT-OP-CREDIT-ED.
       7130-CALC-DB-CB-FIN.
            EXIT.
      *
       7140-CALC-CR-DEP-DEB.
            ADD  1                      TO WS-LINE.
            ADD  1                      TO WS-CDEP.
            ADD  WS-MVTS-MT
                                        TO WS-TOT-MTCR.
            MOVE 'DEPOT GUICHET'
                                        TO WS-LETAT-OP-LIB-ED.
            MOVE WS-MVTS-JJ
                                        TO WS-LETAT-OP-JJ-ED.
            MOVE WS-MVTS-MM
                                        TO WS-LETAT-OP-MM-ED.
            MOVE WS-MVTS-AA
                                        TO WS-LETAT-OP-AA-ED.
            MOVE WS-MVTS-SS
                                        TO WS-LETAT-OP-SS-ED.
            MOVE WS-MVTS-MT
                                        TO WS-LETAT-OP-CREDIT-ED.
            MOVE 0                      TO WS-LETAT-OP-DEBIT-ED.
       7140-CALC-CR-DEP-FIN.
            EXIT.
      *
       7150-CALC-ERR-DEB.
            ADD  1                      TO WS-CERR.
            ADD  WS-MVTS-MT
                                        TO WS-TOT-MT-ERR.
            MOVE WS-MVTS-MT
                                        TO WS-LANO-MONTANT-ED.
            MOVE WS-MVTS-CPTE
                                        TO WS-LANO-NUMCPT-ED.
            MOVE WS-MVTS-CODE
                                        TO WS-LANO-CODEMVT-ED.
       7150-CALC-ERR-FIN.
            EXIT.
      *
       7160-INCR-CLICLOT-DEB.
            COMPUTE WS-SOLDE-CALC = WS-SOLDE-CALC - WS-TOT-MTDB
                   + WS-TOT-MTCR.
            MOVE WS-SOLDE-CALC
                                        TO WS-CPTS-SOLDE.
            ADD  1                      TO WS-CCLICLOT.
       7160-INCR-CLICLOT-FIN.
            EXIT.
      *
       7170-INCR-CLIST-DEB.
            COMPUTE WS-SOLDE-CALC = WS-SOLDE-CALC - WS-TOT-MTDB
                   + WS-TOT-MTCR.
            MOVE WS-SOLDE-CALC
                                        TO WS-CPTS-SOLDE.
            ADD  1                      TO WS-CCLIST.
       7170-INCR-CLIST-FIN.
            EXIT.
      *
       7180-INIT-DCREA-DEB.
            MOVE WS-MVTS-DATE           TO WS-CPTS-DCREA.
       7180-INIT-DCREA-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-PGARDE-ETATCLI-DEB.
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6120-WRITE-FIRST-LINE-CLI-DEB
               THRU  6120-WRITE-FIRST-LINE-CLI-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L3
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN
                     2 TIMES.
      *
            MOVE WS-ENTETE-L5
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L6
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L7
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L8
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
       8000-PGARDE-ETATCLI-FIN.
            EXIT.
      *
       8010-PGARDE-ETATANO-DEB.
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6140-WRITE-FIRST-LINE-ANO-DEB
               THRU  6140-WRITE-FIRST-LINE-ANO-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-ENTETE-L3
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-ENTETE-L4
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-ENTETE-L5
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-ENTETE-L6
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
              THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L7
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L8
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L2
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
              THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
       8010-PGARDE-ETATANO-FIN.
            EXIT.
      *
       8020-DET-ANO-OK-DEB.
            MOVE WS-LANO-OK
                                        TO WS-BUFFER.
            PERFORM  6140-WRITE-FIRST-LINE-ANO-DEB
               THRU  6140-WRITE-FIRST-LINE-ANO-FIN.
       8020-DET-ANO-OK-FIN.
            EXIT.
      *
       8030-FIN-ETATANO-DEB.
            MOVE WS-LANO-L3
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-TOT-MT-ERR
                                        TO WS-LANO-TOTAL-ED.
      *
            MOVE WS-LANO-TOTAL
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-L1
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
       8030-FIN-ETATANO-FIN.
            EXIT.
      *
       8040-IMP-FIN-CPT-DEB.
            MOVE WS-LETAT-FILLER
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-TOT-MTDB
                                        TO WS-LETAT-TOTDB-ED.
      *
            MOVE WS-TOT-MTCR
                                        TO WS-LETAT-TOTCR-ED.
      *
            MOVE WS-LETAT-TOT-OP
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-FILLER
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-SOLD-OP
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
       8040-IMP-FIN-CPT-FIN.
            EXIT.
      *
       8050-IMP-ENT-CLI-DEB.
            MOVE WS-ENTETE-L1
                                        TO WS-BUFFER.
            PERFORM  6120-WRITE-FIRST-LINE-CLI-DEB
               THRU  6120-WRITE-FIRST-LINE-CLI-FIN.
      *
            MOVE WS-LETAT-DATE-PAGE
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-NUMCPT
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-FILLER
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-SOLD-OP
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-FILLER
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-TITRES
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
            MOVE WS-LETAT-FILLER
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
      *
       8050-IMP-ENT-CLI-FIN.
            EXIT.
      *
       8060-IMP-DET-CLI-DEB.
            MOVE WS-LETAT-DETAIL-OP
                                        TO WS-BUFFER.
            PERFORM  6130-WRITE-DET-CLI-DEB
               THRU  6130-WRITE-DET-CLI-FIN.
       8060-IMP-DET-CLI-FIN.
            EXIT.
      *
       8070-IMP-DET-ANO-DEB.
            MOVE WS-LANO-DETAIL
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
       8070-IMP-DET-ANO-FIN.
            EXIT.
      *
       8080-IMP-ENT-ANO-DEB.
            MOVE WS-LANO-L1
                                        TO WS-BUFFER.
            PERFORM  6140-WRITE-FIRST-LINE-ANO-DEB
              THRU  6140-WRITE-FIRST-LINE-ANO-FIN.
      *
            MOVE WS-LANO-TITRES
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
      *
            MOVE WS-LANO-L3
                                        TO WS-BUFFER.
            PERFORM  6150-WRITE-DET-ANO-DEB
               THRU  6150-WRITE-DET-ANO-FIN.
       8080-IMP-ENT-ANO-FIN.
            EXIT.
      *
       8999-STATISTIQUES-DEB.
      *
            DISPLAY WS-LCRE-ASTER.
            DISPLAY WS-LCRE-TITRE.
            DISPLAY WS-LCRE-ASTER.
      *
            MOVE    'NOMBRE DE CLIENTS' TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CCLI             TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE CLIENTS NOUVEAUX'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CCLINEW          TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE CLOTURES'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CCLICLOT         TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE CLIENTS STANDARDS'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CCLIST           TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE MOUVEMENTS'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CMVT             TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE MOUVEMENTS ERRONES'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CERR             TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE RETRAITS'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CRET             TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE CARTES BLEUES'
                                        TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CCB              TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            MOVE    'NOMBRE DE DEPOTS'  TO WS-LCRE-DET-LIB-ED.
            MOVE    WS-CDEP             TO WS-LCRE-DET-TOT-ED.
            DISPLAY WS-LCRE-DETAIL.
      *
            DISPLAY WS-LCRE-ASTER.
      *
       8999-STATISTIQUES-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO44B         *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO44B        *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
