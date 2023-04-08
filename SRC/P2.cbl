      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC462                                   *
      *  NOM DU REDACTEUR : LAMBERT-HUYGHE, ANTIGONE                  *
      *  SOCIETE          : ESTIAC FORMATION                          *
      *  DATE DE CREATION : 30/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * OBJECTIF : PROGRAMME QUI AFFICHE LE MENU DE CONSULTATION DES  *
      *    ARTICLES EN ACCES DIRECT (APPLI GESTART); PRISE EN COMPTE  *
      *    DU CODE ARTICLE DEMANDE, DES TOUCHE FONCTION F1 (AIDE), F3 *
      *    (RETOUR AU MENU PRINCUPAL); GESTION DE TENTATIVE D'EFFACE- *
      *    MENT DE LA MAP, DE TOUCHE INVALIDE, DE CODE ARTICLE INEX-  *
      *    ISTANT                                                     *
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
       PROGRAM-ID.      ARIC462.
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
           COPY ARIN462.
      *    VARIABLES POUR SET L'ATTRIBUT DES ENTREES DE LA MAP
           COPY DFHBMSCA.
      *    VARIABLES DE CONDITIONS D'EXECUTION DE CICS (TOUCHES, CODE
      *    RETOUR, ETC)
           COPY DFHAID.
      *    VARIABLES DE LA COMMAREA
           COPY COMMAREA.
      *    MESSAGES D'EXECUTION
           COPY TABMSG.
      *    TABLE NOMS TRANSACTIONS ET PROGRAMMES
           COPY TABPGMID.
      *    VARIABLE CODE RETOUR
       77  WS-CICS-RC                      PIC S9(4)    COMP.
           88 CICS-OK                      VALUE DFHRESP(NORMAL).
           88 MAPFAIL                      VALUE DFHRESP(MAPFAIL).
           88 ART-NOTFOUND                 VALUE DFHRESP(NOTFND).
      *    VARIABLE POUR MESSAGE NON FORMATE
       77  WS-MSG-FIN                      PIC X(79).
      * VARIABLES DATE ET RESERVE
       77  WS-ABSTIME                      PIC X(15).
       77  WS-LOT-N                        PIC 9.
       01  WS-AFFICHE-LOT.
           05  FILLER                      PIC X(12)
                                           VALUE SPACE.
           05  WS-LIBEL-LOT                PIC X(6).
           05  FILLER                      PIC X(18)
                                           VALUE SPACE.
           05  WS-QTE-LOT                  PIC ZZZZ9.
           05  FILLER                      PIC X(18)
                                           VALUE SPACE.
           05  WS-PRX-LOT                  PIC ZZZZ9,99.
           05  FILLER                      PIC X(12)
                                           VALUE SPACE.
      *
       LINKAGE SECTION.
      * RECUPERATION DE LA COMMAREA
       77  DFHCOMMAREA                  PIC X(4096).
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
           PERFORM 7120-RECUP-COMMAREA-DEB
              THRU 7120-RECUP-COMMAREA-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (AS)
      *---------------------------------------------------------------*
           IF INIT-TRT OR AFF-AIDE
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
           IF LOOP-MENU
              PERFORM 9000-CALL-MENU-DEB
                 THRU 9000-CALL-MENU-FIN
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
      *
           PERFORM 7010-AFF-MAP-DEB
              THRU 7010-AFF-MAP-FIN.
      * SI REPRISE APRES AFFICHAGE D'AIDE, REMPLIR LA MAP AVEC LES
      * ANCIENNES VALEURS
           IF SAVE-MAP
              PERFORM 7020-FILL-MAP-DEB
                 THRU 7020-FILL-MAP-FIN
              PERFORM 7030-FILL-MAP-LOTS-DEB
                 THRU 7030-FILL-MAP-LOTS-FIN
                 VARYING WS-LOT-N FROM 1 BY 1
                 UNTIL WS-LOT-N > WS-NB-LOT
           END-IF.
      *
      *    MOVE 'ENTREE AFF1' TO MMSGO.
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
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)
      *---------------------------------------------------------------*
           EVALUATE EIBAID
              WHEN DFHENTER
                 PERFORM 2000-ENTREE-DEB
                    THRU 2000-ENTREE-FIN
              WHEN DFHPF1
                 PERFORM 2010-PF1-DEB
                    THRU 2010-PF1-FIN
              WHEN DFHPF3
                 PERFORM 2020-PF3-DEB
                    THRU 2020-PF3-FIN
              WHEN DFHCLEAR
                 PERFORM 2030-ALTC-DEB
                    THRU 2030-ALTC-FIN
              WHEN OTHER
                 PERFORM 2040-TFCT-WRONG-DEB
                    THRU 2040-TFCT-WRONG-FIN
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
      *                DESCRIPTION DU COMPOSANT ENTREE                *
      *                ===============================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2000-ENTREE-DEB.
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
           IF MAPFAIL OR MCODEI = SPACE
              PERFORM 3000-ART-VIDE-DEB
                 THRU 3000-ART-VIDE-FIN
           ELSE
              PERFORM 3010-ART-CHOIX-DEB
                 THRU 3010-ART-CHOIX-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
       2000-ENTREE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PF1                    *
      *               ============================                    *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2010-PF1-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
      *    PERFORM 6020-RECEIVE-MAP-DEB
      *       THRU 6020-RECEIVE-MAP-FIN.
      *
           PERFORM 7130-SET-AFFAIDE-DEB
              THRU 7130-SET-AFFAIDE-FIN.
           PERFORM 6030-SEND-HELP-DEB
              THRU 6030-SEND-HELP-FIN.
      *
       2010-PF1-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PF3                    *
      *               ============================                    *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2020-PF3-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7060-PREP-RETOUR-MENU-DEB
              THRU 7060-PREP-RETOUR-MENU-FIN.
      *
       2020-PF3-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ALTC                   *
      *               =============================                   *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2030-ALTC-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7070-PREP-MSG-CLEAR-DEB
              THRU 7070-PREP-MSG-CLEAR-FIN.
           IF SAVE-MAP
              PERFORM 7020-FILL-MAP-DEB
                 THRU 7020-FILL-MAP-FIN
              PERFORM 7030-FILL-MAP-LOTS-DEB
                 THRU 7030-FILL-MAP-LOTS-FIN
                 VARYING WS-LOT-N FROM 1 BY 1
                 UNTIL WS-LOT-N > WS-NB-LOT
           END-IF.
      *
           PERFORM 6000-SEND-FULLMAP-DEB
              THRU 6000-SEND-FULLMAP-FIN.
      *
       2030-ALTC-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT T-FCT-WRONG               *
      *            ====================================               *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       2040-TFCT-WRONG-DEB.
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7080-PREP-MSG-TFCT-WRONG-DEB
              THRU 7080-PREP-MSG-TFCT-WRONG-FIN.
      *
           IF SAVE-MAP
              PERFORM 7020-FILL-MAP-DEB
                 THRU 7020-FILL-MAP-FIN
              PERFORM 7030-FILL-MAP-LOTS-DEB
                 THRU 7030-FILL-MAP-LOTS-FIN
                 VARYING WS-LOT-N FROM 1 BY 1
                 UNTIL WS-LOT-N > WS-NB-LOT
           END-IF.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
       2040-TFCT-WRONG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *              DESCRIPTION DU COMPOSANT ART-VIDE                *
      *              =================================                *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3000-ART-VIDE-DEB.
      *
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7040-EMPTY-MAP-DEB
              THRU 7040-EMPTY-MAP-FIN.
           PERFORM 7050-EMPTY-MAP-LOTS-DEB
              THRU 7050-EMPTY-MAP-LOTS-FIN
              VARYING WS-LOT-N FROM 1 BY 1
              UNTIL WS-LOT-N > WS-NB-LOT.
           PERFORM 7090-PREP-MSG-NOCODE-DEB
              THRU 7090-PREP-MSG-NOCODE-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DE PLUS BAS NIVEAU
      *---------------------------------------------------------------*
      *
       3000-ART-VIDE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ART-CHOIX              *
      *               ==================================              *
      *---------------------------------------------------------------*
      * DESCRIPTION                                                   *
      *---------------------------------------------------------------*
       3010-ART-CHOIX-DEB.
      *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           PERFORM 6040-READ-ART-DEB
              THRU 6040-READ-ART-FIN.
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)
      *---------------------------------------------------------------*
           IF ART-NOTFOUND
                 PERFORM 4000-ART-WRONG-DEB
                    THRU 4000-ART-WRONG-FIN
           ELSE
                 PERFORM 4010-ART-OK-DEB
                    THRU 4010-ART-OK-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
       3010-ART-CHOIX-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      * OPT-WRONG (TRAITEMENT DE PLUS BAS NIVEAU)                     *
      *                                                               *
      *---------------------------------------------------------------*
       4000-ART-WRONG-DEB.
      *
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7040-EMPTY-MAP-DEB
              THRU 7040-EMPTY-MAP-FIN.
           PERFORM 7050-EMPTY-MAP-LOTS-DEB
              THRU 7050-EMPTY-MAP-LOTS-FIN
              VARYING WS-LOT-N FROM 1 BY 1
              UNTIL WS-LOT-N > WS-NB-LOT.
           PERFORM 7100-PREP-MSG-NOTFOUND-DEB
              THRU 7100-PREP-MSG-NOTFOUND-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
       4000-ART-WRONG-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      * OPT-OK (TRAITEMENT DE PLUS BAS NIVEAU)                        *
      *                                                               *
      *---------------------------------------------------------------*
       4010-ART-OK-DEB.
      *
      *
           PERFORM 7000-INIT-MAP-DEB
              THRU 7000-INIT-MAP-FIN.
           PERFORM 7020-FILL-MAP-DEB
              THRU 7020-FILL-MAP-FIN.
           PERFORM 7030-FILL-MAP-LOTS-DEB
              THRU 7030-FILL-MAP-LOTS-FIN
              VARYING WS-LOT-N FROM 1 BY 1
              UNTIL WS-LOT-N > WS-NB-LOT.
      *
           PERFORM 7110-SET-SAVEMAP-DEB
              THRU 7110-SET-SAVEMAP-FIN.
      *
           PERFORM 6010-SEND-LMAP-DEB
              THRU 6010-SEND-LMAP-FIN.
      *
       4010-ART-OK-FIN.
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
                SEND MAP ('ARIM462')
                MAPSET   ('ARIN462')
                FROM     (ARIM462O)
                ERASE
                CURSOR
                RESP     (WS-CICS-RC)
           END-EXEC.
      *
           IF WS-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP COMPLETE'
                                            TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-SEND-FULLMAP-FIN.
           EXIT.
      *
       6010-SEND-LMAP-DEB.
           EXEC CICS
                SEND MAP ('ARIM462')
                MAPSET   ('ARIN462')
                FROM     (ARIM462O)
                ERASEAUP
                CURSOR
                DATAONLY
                RESP     (WS-CICS-RC)
           END-EXEC.
      *
           IF WS-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP LOGIQUE'
                                            TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-SEND-LMAP-FIN.
           EXIT.
      *
       6020-RECEIVE-MAP-DEB.
           EXEC CICS
                RECEIVE MAP ('ARIM462')
                MAPSET      ('ARIN462')
                INTO        (ARIM462O)
                RESP        (WS-CICS-RC)
           END-EXEC.
      *
           IF NOT (WS-CICS-RC = DFHRESP(NORMAL)
                   OR WS-CICS-RC = DFHRESP(MAPFAIL))
              MOVE 'ERREUR RECEPTION MAP'
                                            TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-RECEIVE-MAP-FIN.
           EXIT.
      *
       6030-SEND-HELP-DEB.
           EXEC CICS
                SEND MAP    ('ARIMHP2')
                MAPSET      ('ARIN462')
                ERASE
                RESP        (WS-CICS-RC)
           END-EXEC.
      *
           IF NOT WS-CICS-RC = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP AIDE'
                                            TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-SEND-HELP-FIN.
           EXIT.
      *
       6040-READ-ART-DEB.
           EXEC CICS READ
                FILE   ('ART0406')
                RIDFLD (MCODEI)
                INTO   (WS-ENR-SAV)
                RESP   (WS-CICS-RC)
           END-EXEC.
      *
           IF NOT (CICS-OK OR ART-NOTFOUND)
              MOVE 'ERREUR LECTURE ARTICLE'
                                            TO WS-MSG-FIN
      *       STRING 'ERREUR LECTURE ARTICLE; RC : '
      *              DELIMITED BY SIZE
      *              WS-CICS-RC
      *              DELIMITED BY SIZE
      *              INTO WS-MSG-FIN
      *       END-STRING
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-READ-ART-FIN.
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
           MOVE LOW-VALUE                   TO ARIM462O.
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
      * POSITIONNEMENT DU CURSEUR
           MOVE -1                          TO MCODEL.
      * ASSIGNATION NUM. TERMINAL ET ID TRANSACTION
           MOVE EIBTRNID                    TO MTRANI.
           MOVE EIBTRMID                    TO MTERMI.
      * ASSIGNATION NUM. TASK
           MOVE EIBTASKN                    TO MTASKI.
      *
       7000-INIT-MAP-FIN.
           EXIT.
      *
       7010-AFF-MAP-DEB.
           SET  AFF-MAP                     TO TRUE.
           MOVE SPACE                       TO MCODEI.
       7010-AFF-MAP-FIN.
           EXIT.
      *
       7020-FILL-MAP-DEB.
           MOVE WS-CODE                     TO MCODEI.
           MOVE WS-LIB                      TO MLIBELI.
           MOVE WS-CATEG                    TO MCATEGI.
           MOVE WS-FOUR                     TO MFOURI.
           MOVE WS-APPRO                    TO MAPPROI.
           MOVE WS-NB-LOT                   TO MNLOTI.
           MOVE WS-QTE                      TO MQTSTKI.
           MOVE WS-ALERT                    TO MQTALEI.
       7020-FILL-MAP-FIN.
           EXIT.
      *
       7030-FILL-MAP-LOTS-DEB.
           MOVE WS-TLOT-NUM(WS-LOT-N)       TO WS-LIBEL-LOT.
           MOVE WS-TLOT-QTE(WS-LOT-N)       TO WS-QTE-LOT.
           MOVE WS-TLOT-PXU(WS-LOT-N)       TO WS-PRX-LOT.
           MOVE WS-AFFICHE-LOT              TO MLOTI(WS-LOT-N).
       7030-FILL-MAP-LOTS-FIN.
           EXIT.
      *
       7040-EMPTY-MAP-DEB.
           MOVE SPACE                       TO MCODEI
                                               MLIBELI
                                               MCATEGI
                                               MFOURI
                                               MAPPROI
                                               MNLOTI
                                               MQTSTKI
                                               MQTALEI.
       7040-EMPTY-MAP-FIN.
           EXIT.
      *
       7050-EMPTY-MAP-LOTS-DEB.
           MOVE SPACE                       TO MLOTI(WS-LOT-N).
       7050-EMPTY-MAP-LOTS-FIN.
           EXIT.
      *
       7060-PREP-RETOUR-MENU-DEB.
           SET  INIT-TRT                    TO TRUE.
           SET  LOOP-MENU                   TO TRUE.
           SET  RESET-MAP                   TO TRUE.
           MOVE LOW-VALUE                   TO WS-ENR-SAV.
           MOVE WS-SPG(1)                   TO WS-PROG.
       7060-PREP-RETOUR-MENU-FIN.
           EXIT.
      *
       7070-PREP-MSG-CLEAR-DEB.
           MOVE SPACES                      TO MMSGO.
           MOVE WS-MSG(2)                   TO MMSGO.
       7070-PREP-MSG-CLEAR-FIN.
           EXIT.
      *
       7080-PREP-MSG-TFCT-WRONG-DEB.
           MOVE SPACES                      TO MMSGO.
           MOVE WS-MSG(1)                   TO MMSGO.
       7080-PREP-MSG-TFCT-WRONG-FIN.
           EXIT.
      *
       7090-PREP-MSG-NOCODE-DEB.
           MOVE SPACES                      TO MMSGO.
           MOVE WS-MSG(6)                   TO MMSGO.
           SET  RESET-MAP                   TO TRUE.
       7090-PREP-MSG-NOCODE-FIN.
           EXIT.
      *
       7100-PREP-MSG-NOTFOUND-DEB.
           MOVE SPACES                      TO MMSGO.
           MOVE WS-MSG(27)                  TO MMSGO.
           SET  RESET-MAP                   TO TRUE.
       7100-PREP-MSG-NOTFOUND-FIN.
           EXIT.
      *
       7110-SET-SAVEMAP-DEB.
           SET  SAVE-MAP                    TO TRUE.
           MOVE SPACE                       TO MMSGO.
       7110-SET-SAVEMAP-FIN.
           EXIT.
      *
       7120-RECUP-COMMAREA-DEB.
           MOVE DFHCOMMAREA                 TO WS-COMMAREA.
           MOVE 'A461'                      TO WS-TASK.
       7120-RECUP-COMMAREA-FIN.
           EXIT.
      *
       7130-SET-AFFAIDE-DEB.
           SET AFF-AIDE                     TO TRUE.
       7130-SET-AFFAIDE-FIN.
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
       9000-CALL-MENU-DEB.
            EXEC CICS XCTL
                 PROGRAM     (WS-PROG)
                 COMMAREA    (WS-COMMAREA)
                 RESP        (WS-CICS-RC)
            END-EXEC.
      *
            IF NOT WS-CICS-RC = DFHRESP(NORMAL)
               MOVE 'ERREUR APPEL MENU'     TO WS-MSG-FIN
      *        STRING 'ERREUR APPEL MENU; RC : '
      *                      DELIMITED BY SIZE
      *               WS-CICS-RC
      *                      DELIMITED BY SIZE
      *          INTO WS-MSG-FIN
      *        END-STRING.
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       9000-CALL-MENU-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
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
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-RETURN-TRANSID-DEB.
      * FIN TEMPORAIRE DU PROGRAMME
           EXEC CICS
                RETURN
                TRANSID   (WS-TASK)
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
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
