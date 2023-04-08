      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC461                                   *
      *  NOM DU REDACTEUR : LAMBERT-HUYGHE, ANTIGONE                  *
      *  SOCIETE          : ESTIAC FORMATION                          *
      *  DATE DE CREATION : 23/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * OBJECTIF : PROGRAMME QUI AFFICHE LE MENU PRINCIPAL DE L'APP.  *
      *    GESTARTS; GESTION DU PANEL DE CHOIX, DES TOUCHES INVALIDES,*
      *    DES RESULTATS DES CHOIX                                    *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/AAAA    !                                               *
      *               !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIC461.
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
      *          --------------------------------------------------
      *               F-ETATCLI-S : FICHIER D'ETAT DES CLIENTS
      *          --------------------------------------------------
      * NOM DE FICHIER INTERNE : F-ETATCLI-S
      * DDNAME : INP001
      *          -------------------------------------------
      *    SELECT  F-ETATCLI-S ASSIGN TO ETATCLI
      *            FILE STATUS     IS WS-FS-ETATCLI-S.
      *          -------------------------------------------
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
      *-------------------FICHIER ETAT CLIENT EN SORTIE---------------*
      * LONGUEUR ENREGISTREMENT = 80
      *---------------------------------------------------------------*
      *FD  F-ETATCLI-S
      *    RECORDING MODE IS F.
      *
      *-------------------DESCRIPTION DE L'ENREGISTREMENT-------------*
      *01  FS-ENRG-ETATCLI                 PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      * VARIABLES DES INSTRUCTION CICS
      *    VARIABLES LOGIQUES DE LA MAP
           COPY ARIN461.
      *    VARIABLES POUR SET L'ATTRIBUT DES ENTREES DE LA MAP
           COPY DFHBMSCA.
      *    VARIABLES DE CONDITIONS D'EXECUTION DE CICS (TOUCHES, CODE
      *    RETOUR, ETC)
           COPY DFHAID.
      *    VARIABLES DE LA COMMAREA
           COPY COMMAREA.
      *    MESSAGES D'EXECUTION
           COPY TABMSG.
      *    TABLES DES NOMS DES TRANSACTIONS ET DES PROGRAMMES
      *    COPY TABPGMID.
       01  WS-TABNOM-PROG.
           05  FILLER                    PIC X(8) VALUE 'ARIC461'.
           05  FILLER                    PIC X(8) VALUE 'ARIC462'.
           05  FILLER                    PIC X(8) VALUE 'ARIC463'.
           05  FILLER                    PIC X(8) VALUE 'ARIC464'.
           05  FILLER                    PIC X(8) VALUE 'ARIC465'.
           05  FILLER                    PIC X(8) VALUE 'ARIC466'.
           05  FILLER                    PIC X(8) VALUE 'ARIC467'.
       01  FILLER REDEFINES WS-TABNOM-PROG.
           05  WS-SPG                    PIC X(8) OCCURS 7.
       77  WS-PROG                       PIC X(8).
      *    VARIABLE CODE RETOUR
       77  WS-CICS-RC                    PIC S9(4)    COMP.
      *    VARIABLE POUR MESSAGE NON FORMATE
       77  WS-MSG-FIN                    PIC X(79).
      * VARIABLES DATE ET RESERVE
       77  WS-ABSTIME                    PIC X(15).
       77  WS-STATUT-TRANS               PIC X.
           88  FIN-TRANS                 VALUE 'F'.
           88  TRANS-ON                  VALUE 'O'.
      *
       77  WS-PGM-NUM                    PIC 9.
       77  WS-CHOIX-TEST                 PIC X.
           88  CHOIX-OK                  VALUE '1' THRU '6'.
      *
       LINKAGE SECTION.
      * RECUPERATION DE LA COMMAREA
       77  DFHCOMMAREA                    PIC X(4096).
      *
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *                                                               *
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANT-FIN                   *
      *                                                               *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *                                                               *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *                                                               *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PRINCIPAL              *
      *               ==================================              *
      *---------------------------------------------------------------*
      * DESCRIPTION PRINCIPAL
      *---------------------------------------------------------------*
      * DEBUT DU PROGRAMME
      *---------------------------------------------------------------*
       0000-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           PERFORM 7120-SET-CONTINUETRANS-DEB
              THRU 7120-SET-CONTINUETRANS-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (AS)
      *---------------------------------------------------------------*
           IF EIBCALEN = 0 OR INIT-TRT
              PERFORM 1000-AFF1-DEB
                 THRU 1000-AFF1-FIN
           ELSE
              PERFORM 1010-AFFN-DEB
                 THRU 1010-AFFN-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
           IF FIN-TRANS
              PERFORM 9999-FIN-PROGRAMME-DEB
                 THRU 9999-FIN-PROGRAMME-FIN
           ELSE
              PERFORM 9999-RETURN-TRANSID-DEB
                 THRU 9999-RETURN-TRANSID-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU PROGRAMME
      *---------------------------------------------------------------*
       0000-PRINCIPAL-FIN.
           STOP RUN.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT AFF1                   *
      *               =============================                   *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       1000-AFF1-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7010-SET-MENU-DEB
              THRU 7010-SET-MENU-FIN.
      *
           PERFORM 6000-SEND-FULLMAP-DEB
              THRU 6000-SEND-FULLMAP-FIN.
      *
      *---------------------------------------------------------------*
      * FIN DU PARAGRAPHE
      *---------------------------------------------------------------*
       1000-AFF1-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT AFFN                   *
      *               =============================                   *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       1010-AFFN-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE DE GAUCHE)
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)
      *---------------------------------------------------------------*
           EVALUATE TRUE
              WHEN LOOP-MENU
                   PERFORM 2010-AFF-MENU-DEB
                      THRU 2010-AFF-MENU-FIN
              WHEN LOOP-SPG
                   PERFORM 2000-SOUS-PROG-DEB
                      THRU 2000-SOUS-PROG-FIN
              WHEN OTHER
                   PERFORM 2020-AIG-WRONG-DEB
                      THRU 2020-AIG-WRONG-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DE GAUCHE)
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * FIN DU PARAGRAPHE
      *---------------------------------------------------------------*
       1010-AFFN-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *             DESCRIPTION DU COMPOSANT SOUS-PROG                *
      *             ==================================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2000-SOUS-PROG-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7020-PREP-CALL-SPG-DEB
              THRU 7020-PREP-CALL-SPG-FIN.
      *
           IF WS-AIG = '1'
              PERFORM 9000-CALL-SPG-DEB
                 THRU 9000-CALL-SPG-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU PARAGRAPHE
      *---------------------------------------------------------------*
       2000-SOUS-PROG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *             DESCRIPTION DU COMPOSANT AFF-MENU                 *
      *             =================================                 *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2010-AFF-MENU-DEB.
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE DE GAUCHE)
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)
      *---------------------------------------------------------------*
           EVALUATE EIBAID
              WHEN DFHENTER
                 PERFORM 3000-ENTREE-DEB
                    THRU 3000-ENTREE-FIN
              WHEN DFHPF3
                 PERFORM 3010-PF3-DEB
                    THRU 3010-PF3-FIN
              WHEN DFHCLEAR
                 PERFORM 3020-ALTC-DEB
                    THRU 3020-ALTC-FIN
              WHEN OTHER
                 PERFORM 3030-TFCT-WRONG-DEB
                    THRU 3030-TFCT-WRONG-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DE GAUCHE)
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * FIN DU PARAGRAPHE
      *---------------------------------------------------------------*
       2010-AFF-MENU-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *             DESCRIPTION DU COMPOSANT AIG-WRONG                *
      *             ==================================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2020-AIG-WRONG-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7080-PREP-MSG-AIG-WRONG-DEB
              THRU 7080-PREP-MSG-AIG-WRONG-FIN.
      *
           PERFORM 9999-ERREUR-PROGRAMME-DEB
              THRU 9999-ERREUR-PROGRAMME-FIN.
      *
      *---------------------------------------------------------------*
      * FIN DU PARAGRAPHE
      *---------------------------------------------------------------*
       2020-AIG-WRONG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                DESCRIPTION DU COMPOSANT ENTREE                *
      *                ===============================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3000-ENTREE-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           PERFORM 6020-RECEIVE-MAP-DEB
              THRU 6020-RECEIVE-MAP-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE SIMPLE)
      *---------------------------------------------------------------*
           IF WS-CICS-RC = DFHRESP(MAPFAIL) OR
              MCHOIXI = ' '
              PERFORM 4000-CHOIX-VIDE-DEB
                 THRU 4000-CHOIX-VIDE-FIN
           ELSE
              PERFORM 4010-CHOIX-FAIT-DEB
                 THRU 4010-CHOIX-FAIT-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
       3000-ENTREE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PF3                    *
      *               ============================                    *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3010-PF3-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7030-PREP-FINTRS-DEB
              THRU 7030-PREP-FINTRS-FIN.
      *
       3010-PF3-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ALTC                   *
      *               =============================                   *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3020-ALTC-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7040-PREP-MSG-CLEAR-DEB
              THRU 7040-PREP-MSG-CLEAR-FIN.
      *
           PERFORM 6000-SEND-FULLMAP-DEB
              THRU 6000-SEND-FULLMAP-FIN.
      *
       3020-ALTC-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT T-FCT-WRONG               *
      *            ====================================               *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3030-TFCT-WRONG-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7050-PREP-MSG-TFCT-WRONG-DEB
              THRU 7050-PREP-MSG-TFCT-WRONG-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
       3030-TFCT-WRONG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT CHOIX-VIDE                *
      *            ===================================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       4000-CHOIX-VIDE-DEB.
      *
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7060-PREP-MSG-NOCHOICE-DEB
              THRU 7060-PREP-MSG-NOCHOICE-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
      *
       4000-CHOIX-VIDE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT CHOIX-FAIT             *
      *               ===================================             *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       4010-CHOIX-FAIT-DEB.
      *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           PERFORM 7090-KEEP-CHOIX-DEB
              THRU 7090-KEEP-CHOIX-FIN.
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)
      *---------------------------------------------------------------*
           IF CHOIX-OK
                 PERFORM 5010-OPT-OK-DEB
                    THRU 5010-OPT-OK-FIN
           ELSE
                 PERFORM 5000-OPT-WRONG-DEB
                    THRU 5000-OPT-WRONG-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
       4010-CHOIX-FAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      * OPT-WRONG (TRAITEMENT DE PLUS BAS NIVEAU)                     *
      *                                                               *
      *---------------------------------------------------------------*
       5000-OPT-WRONG-DEB.
      *
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7070-PREP-MSG-OPT-WRONG-DEB
              THRU 7070-PREP-MSG-OPT-WRONG-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
       5000-OPT-WRONG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      * OPT-OK (TRAITEMENT DE PLUS BAS NIVEAU)                        *
      *---------------------------------------------------------------*
       5010-OPT-OK-DEB.
      *
           IF WS-CHOIX = '1'
              PERFORM 7100-PREP-FIRST-SPG-DEB
                 THRU 7100-PREP-FIRST-SPG-FIN
      *
              PERFORM 9000-CALL-SPG-DEB
                 THRU 9000-CALL-SPG-FIN
           ELSE
              PERFORM 7000-INIT-MAP-DEB
                 THRU 7000-INIT-MAP-FIN
              PERFORM 7110-PREP-MSG-CHOIX-DEB
                 THRU 7110-PREP-MSG-CHOIX-FIN
      *
              PERFORM 6010-SEND-LMAP-DEB
                 THRU 6010-SEND-LMAP-FIN
           END-IF.
      *
       5010-OPT-OK-FIN.
           EXIT.
      *
      *===============================================================*
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *                                                               *
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *                                                               *
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.
      *    EXIT.
      *
       6000-SEND-FULLMAP-DEB.
           EXEC CICS
                SEND MAP ('ARIM461')
                MAPSET   ('ARIN461')
                FROM     (ARIM461O)
                ERASE
                CURSOR
                RESP     (WS-CICS-RC)
           END-EXEC.
      *
           IF WS-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP COMPLETE' TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-SEND-FULLMAP-FIN.
           EXIT.
      *
       6010-SEND-LMAP-DEB.
           EXEC CICS
                SEND MAP ('ARIM461')
                MAPSET   ('ARIN461')
                FROM     (ARIM461O)
                ERASEAUP
                CURSOR
                DATAONLY
                RESP     (WS-CICS-RC)
           END-EXEC.
      *
           IF WS-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP LOGIQUE' TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-SEND-LMAP-FIN.
           EXIT.
      *
       6020-RECEIVE-MAP-DEB.
           EXEC CICS
                RECEIVE MAP ('ARIM461')
                MAPSET      ('ARIN461')
                INTO        (ARIM461O)
                RESP        (WS-CICS-RC)
           END-EXEC.
      *
           IF NOT (WS-CICS-RC = DFHRESP(NORMAL)
                   OR WS-CICS-RC = DFHRESP(MAPFAIL))
              MOVE 'ERREUR RECEPTION MAP' TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-RECEIVE-MAP-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
       7000-INIT-MAP-DEB.
      * INITIALISATION VIDE DE LA MAP LOGIQUE
           MOVE LOW-VALUE          TO ARIM461O.
      * POSITIONNEMENT DU CURSEUR
           MOVE -1                 TO MCHOIXL.
      * ASSIGNATION NUM. TERMINAL ET ID TRANSACTION
           MOVE EIBTRNID           TO MTRANI.
           MOVE EIBTRMID           TO MTERMI.
      * RECUPERATION DE LA DATE VIA CICS
           EXEC CICS
                ASKTIME
                ABSTIME (WS-ABSTIME)
           END-EXEC.
      * FORMATAGE DE LA DATE
           EXEC CICS
                FORMATTIME
                ABSTIME  (WS-ABSTIME)
                DDMMYYYY (MDATEI)
                DATESEP  ('/')
           END-EXEC.
      * ASSIGNATION NUM. TASK
           MOVE EIBTASKN           TO MTASKI.
      *
       7000-INIT-MAP-FIN.
           EXIT.
      *
       7010-SET-MENU-DEB.
           MOVE '0'                TO WS-AIG.
           SET  AFF-MAP            TO TRUE.
       7010-SET-MENU-FIN.
           EXIT.
      *
       7020-PREP-CALL-SPG-DEB.
           ADD  1                  TO WS-AIG
               GIVING WS-PGM-NUM.
           MOVE WS-SPG(WS-PGM-NUM) TO WS-PROG.
       7020-PREP-CALL-SPG-FIN.
           EXIT.
      *
       7030-PREP-FINTRS-DEB.
           SET  FIN-TRANS          TO TRUE.
           MOVE WS-MSG(26)         TO WS-MSG-FIN.
       7030-PREP-FINTRS-FIN.
           EXIT.
      *
       7040-PREP-MSG-CLEAR-DEB.
           MOVE SPACES             TO MMSGO.
           MOVE WS-MSG(2)          TO MMSGO.
       7040-PREP-MSG-CLEAR-FIN.
           EXIT.
      *
       7050-PREP-MSG-TFCT-WRONG-DEB.
           MOVE SPACES             TO MMSGO.
           MOVE WS-MSG(1)          TO MMSGO.
       7050-PREP-MSG-TFCT-WRONG-FIN.
           EXIT.
      *
       7060-PREP-MSG-NOCHOICE-DEB.
           MOVE SPACES             TO MMSGO.
           MOVE WS-MSG(24)         TO MMSGO.
           SET LOOP-MENU           TO TRUE.
       7060-PREP-MSG-NOCHOICE-FIN.
           EXIT.
      *
       7070-PREP-MSG-OPT-WRONG-DEB.
           MOVE SPACES             TO MMSGO.
           MOVE WS-MSG(25)         TO MMSGO.
       7070-PREP-MSG-OPT-WRONG-FIN.
           EXIT.
      *
       7080-PREP-MSG-AIG-WRONG-DEB.
           STRING 'PROBLEME D''AIGUILLAGE, WS-AIG='
                  DELIMITED BY SIZE
                  WS-AIG
                  DELIMITED BY SIZE
              INTO WS-MSG-FIN
           END-STRING.
       7080-PREP-MSG-AIG-WRONG-FIN.
           EXIT.
      *
       7090-KEEP-CHOIX-DEB.
           MOVE MCHOIXI            TO WS-CHOIX.
           MOVE MCHOIXI            TO WS-CHOIX-TEST.
       7090-KEEP-CHOIX-FIN.
           EXIT.
      *
       7100-PREP-FIRST-SPG-DEB.
           MOVE WS-CHOIX           TO WS-AIG.
           SET  INIT-TRT           TO TRUE.
           ADD  1                  TO WS-AIG
               GIVING WS-PGM-NUM.
           MOVE WS-SPG(WS-PGM-NUM) TO WS-PROG.
       7100-PREP-FIRST-SPG-FIN.
           EXIT.
      *
       7110-PREP-MSG-CHOIX-DEB.
           MOVE SPACES             TO MMSGO.
           STRING
                  'VOUS AVEZ CHOISI L''OPTION '
                  DELIMITED BY SIZE
                  WS-CHOIX
                  DELIMITED BY SIZE
              INTO MMSGO
           END-STRING.
           MOVE LOW-VALUE          TO MCHOIXO.
       7110-PREP-MSG-CHOIX-FIN.
           EXIT.
      *
       7120-SET-CONTINUETRANS-DEB.
           SET TRANS-ON            TO TRUE.
           MOVE DFHCOMMAREA        TO WS-COMMAREA.
       7120-SET-CONTINUETRANS-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
       9000-CALL-SPG-DEB.
           EXEC CICS XCTL
                PROGRAM     (WS-PROG)
                COMMAREA    (WS-COMMAREA)
                RESP        (WS-CICS-RC)
           END-EXEC.
      *
           IF NOT WS-CICS-RC = DFHRESP(NORMAL)
              MOVE 'ERREUR APPEL SOUS-PROGRAMME'
                                             TO WS-MSG-FIN
      *       STRING 'ERREUR APPEL SOUS-PROGRAMME; RC : '
      *                     DELIMITED BY SIZE
      *              WS-CICS-RC
      *                     DELIMITED BY SIZE
      *         INTO WS-MSG-FIN
      *       END-STRING.
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       9000-CALL-SPG-FIN.
           EXIT.
      *
       9010-ABEND-TRANS-DEB.
           EXEC CICS ABEND
                     NODUMP
           END-EXEC.
       9010-ABEND-TRANS-FIN.
           EXIT.
      *
       9999-FIN-PROGRAMME-DEB.
      * AFFICHAGE DU MESSAGE DE FIN DE TRANSACTION
           EXEC CICS
                SEND FROM (WS-MSG-FIN)
                ERASE
                RESP      (WS-CICS-RC)
           END-EXEC.
      * FIN DU PROGRAMME
           EXEC CICS
                RETURN
                RESP      (WS-CICS-RC)
           END-EXEC.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-RETURN-TRANSID-DEB.
      * FIN TEMPORAIRE DU PROGRAMME
           EXEC CICS
                RETURN
                TRANSID   (MTRANI)
                COMMAREA  (WS-COMMAREA)
                RESP      (WS-CICS-RC)
           END-EXEC.
       9999-RETURN-TRANSID-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
      * AFFICHAGE DU MESSAGE D'ERREUR
           EXEC CICS
                SEND FROM (WS-MSG-FIN)
                ERASE
                RESP      (WS-CICS-RC)
           END-EXEC.
      *
      * FIN DU PROGRAMME
           EXEC CICS
                RETURN
                RESP      (WS-CICS-RC)
           END-EXEC.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
