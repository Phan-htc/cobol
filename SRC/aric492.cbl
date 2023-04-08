      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC492                                   *
      *  NOM DU REDACTEUR : PHAN CHARLIE                              *
      *  SOCIETE          : TRICOPHO                                  *
      *  DATE DE CREATION : --/--/----                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * LE PROGRAMME PERMET LA CONSULTATION EN ACCES DIRECT.          *
      * ACCES A UN CLUNSTER KSDS.                                     *
      * UTILISATION D'UNE DEUXIEME MAP.                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    §                                               *
      *               §                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. ARIC492
      *
      *===============================================================*
      *           NE PAS MODIFIER LA PARTIE ENCADREE DU CODE          *
      *===============================================================*
      *
      *                  ==============================               *
      *=================<    ENVIRONMENT    DIVISION   >==============*
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
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
      *
      *                  ==============================               *
      *=================<         DATA      DIVISION   >==============*
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
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
       01  WS-DATE                       PIC X(10).
       01  WS-ASKTIME                    PIC S9(15) COMP-3.
       01  WS-RC                         PIC S9(4)  COMP.
           88 CICS-OK                    VALUE DFHRESP(NORMAL).
           88 MAPFAIL                    VALUE DFHRESP(MAPFAIL).
           88 ART-NOTFND                 VALUE DFHRESP(NOTFND).
       77  WS-MCHOIXT                    PIC X.
       77  WS-MSG-FIN                    PIC X(79).
      *
       77  WS-STATUT-TRANS               PIC X.
           88  FIN-TRANS                 VALUE 'F'.
           88  TRANS-OK                  VALUE 'O'.
       77  WS-N-LOT                      PIC 9.
       01  WS-AFFICHE-LOT.
           05  FILLER                    PIC X(12)
                                         VALUE SPACE.
           05  WS-LIBEL-LOT              PIC X(6).
           05  FILLER                    PIC X(18)
                                         VALUE SPACE.
           05  WS-QTE-LOT                PIC ZZZZ9.
           05  FILLER                    PIC X(18)
                                         VALUE SPACE.
           05  WS-PRX-LOT                PIC ZZZZ9,99.
           05  FILLER                    PIC X(12)
                                         VALUE SPACE.
       01  WS-PROG                       PIC X(8).
       01  WS-TABNOM-PROG.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC491'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC492'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC493'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC494'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC495'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC496'.
           05  FILLER                    PIC X(8)
                                         VALUE 'ARIC497'.
       01  FILLER REDEFINES WS-TABNOM-PROG.
           05  WS-SPG                    PIC X(8) OCCURS 7.
      *
      *===============================================================*
      *             COPY - INSERTION DE SEQUENCES DE SOURCE           *
      *===============================================================*
      * TEST DES TOUCHES FONCTION
           COPY DFHAID.
      * MODIFICATION DYNAMIQUE DES ATTRIBUTS DE MAP
           COPY DFHBMSCA.
      * TABLE DES MESSAGES
           COPY TABMSG.
      * VARIABLE LOGIQUE DE LA MAP
           COPY ARIN492.
      * VARIABLE COMMAREA
           COPY COMMAREA.
      * TABLE ARTICLE
           COPY ARTICLE.
      *
      *================
       LINKAGE SECTION.
      *================
      *
       01  DFHCOMMAREA                   PIC X(4096).
      *
      *                  ==============================               *
      *=================<    PROCEDURE      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXYY-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANT-FIN                   *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *---------------------------------------------------------------*
      *                                                               *
      *---------------------------------------------------------------*
       0000-PROGRAMME-DEB.
      *
           PERFORM 7070-RECUP-COMMAREA-DEB
              THRU 7070-RECUP-COMMAREA-FIN.
      *
           IF INIT-TRT OR AFF-AIDE
              PERFORM 1000-PRE-AFFICHAGE-DEB
                 THRU 1000-PRE-AFFICHAGE-FIN
           ELSE
              PERFORM 1010-N-AFFICHAGE-DEB
                 THRU 1010-N-AFFICHAGE-FIN
           END-IF.
      *
           IF LOOP-MENU
              PERFORM 9000-MENU-DEB
                 THRU 9000-MENU-FIN
           ELSE
              PERFORM 9999-FIN-RETURN-TRANSID-DEB
                 THRU 9999-FIN-RETURN-TRANSID-FIN
           END-IF.
      *
       0000-PROGRAMME-FIN.
            STOP RUN.
      *
       1000-PRE-AFFICHAGE-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
           PERFORM 7040-AFFICHAGE-MAP-DEB
              THRU 7040-AFFICHAGE-MAP-FIN.
      *
           IF SAVE-MAP
              PERFORM 7050-TRANSFERT-INFO-DEB
                 THRU 7050-TRANSFERT-INFO-FIN
              PERFORM 7060-TRANSFERT-INFO-TAB-DEB
                 THRU 7060-TRANSFERT-INFO-TAB-FIN
              VARYING WS-N-LOT FROM 1 BY 1
                UNTIL WS-N-LOT > WS-NB-LOT
           END-IF.
      *
           PERFORM 6000-AFFICHAGE-FULLMAP-DEB
              THRU 6000-AFFICHAGE-FULLMAP-FIN.
      *
       1000-PRE-AFFICHAGE-FIN.
           EXIT.
      *
       1010-N-AFFICHAGE-DEB.
      *
           EVALUATE EIBAID
      *
               WHEN DFHENTER
                    PERFORM 2000-VALIDATION-DEB
                       THRU 2000-VALIDATION-FIN
               WHEN DFHPF1
                    PERFORM 2010-AIDE-DEB
                       THRU 2010-AIDE-FIN
               WHEN DFHPF3
                    PERFORM 2020-RETOUR-DEB
                       THRU 2020-RETOUR-FIN
               WHEN DFHCLEAR
                    PERFORM 2030-EFFACEMENT-ECRAN-DEB
                       THRU 2030-EFFACEMENT-ECRAN-FIN
               WHEN OTHER
                    PERFORM 2040-ERREUR-TOUCHE-DEB
                       THRU 2040-ERREUR-TOUCHE-FIN
           END-EVALUATE.
      *
       1010-N-AFFICHAGE-FIN.
           EXIT.
      *
       2000-VALIDATION-DEB.
      *
      * OREILLETTE GAUCHE
           PERFORM 6020-RECEIVE-MAP-DEB
              THRU 6020-RECEIVE-MAP-FIN.
      *
           IF MAPFAIL OR MCODEI = SPACE
              PERFORM 3000-CHOIX-VIDE-DEB
                 THRU 3000-CHOIX-VIDE-FIN
           ELSE
              PERFORM 3010-CHOIX-OK-DEB
                 THRU 3010-CHOIX-OK-FIN
           END-IF.
      *
       2000-VALIDATION-FIN.
           EXIT.
      *
       2010-AIDE-DEB.
      *
           PERFORM 7080-AFFICHAGE-AIDE-DEB
              THRU 7080-AFFICHAGE-AIDE-FIN.
           PERFORM 6040-AFFICHAGE-AIDE-DEB
              THRU 6040-AFFICHAGE-AIDE-FIN.
      *
       2010-AIDE-FIN.
           EXIT.
       2020-RETOUR-DEB.
      *
           PERFORM 7090-RETOUR-MENU-DEB
              THRU 7090-RETOUR-MENU-FIN.
           PERFORM 9000-MENU-DEB
              THRU 9000-MENU-FIN.
      *
       2020-RETOUR-FIN.
           EXIT.
      *
       2030-EFFACEMENT-ECRAN-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
           PERFORM 7010-MSG-EFF-ECRAN-N-AUTOR-DEB
              THRU 7010-MSG-EFF-ECRAN-N-AUTOR-FIN.
      *
           IF SAVE-MAP
              PERFORM 7050-TRANSFERT-INFO-DEB
                 THRU 7050-TRANSFERT-INFO-FIN
              PERFORM 7060-TRANSFERT-INFO-TAB-DEB
                 THRU 7060-TRANSFERT-INFO-TAB-FIN
              VARYING WS-N-LOT FROM 1 BY 1
                UNTIL WS-N-LOT > WS-NB-LOT
           END-IF.
      *
           PERFORM 6000-AFFICHAGE-FULLMAP-DEB
              THRU 6000-AFFICHAGE-FULLMAP-FIN.
      *
       2030-EFFACEMENT-ECRAN-FIN.
           EXIT.
      *
       2040-ERREUR-TOUCHE-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
      *
           IF SAVE-MAP
              PERFORM 7050-TRANSFERT-INFO-DEB
                 THRU 7050-TRANSFERT-INFO-FIN
              PERFORM 7060-TRANSFERT-INFO-TAB-DEB
                 THRU 7060-TRANSFERT-INFO-TAB-FIN
              VARYING WS-N-LOT FROM 1 BY 1
                UNTIL WS-N-LOT > WS-NB-LOT
           END-IF.
      *
           PERFORM 7030-MSG-FCT-INVALIDE-DEB
              THRU 7030-MSG-FCT-INVALIDE-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       2040-ERREUR-TOUCHE-FIN.
           EXIT.
      *
       3000-CHOIX-VIDE-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
           PERFORM 7020-MSG-ZERO-RENS-DEB
              THRU 7020-MSG-ZERO-RENS-FIN.
           PERFORM 7100-MAP-VIDE-DEB
              THRU 7100-MAP-VIDE-FIN
           VARYING WS-N-LOT FROM 1 BY 1
             UNTIL WS-N-LOT > 5.
           PERFORM 7110-TAB-VIDE-DEB
              THRU 7110-TAB-VIDE-FIN
      *
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       3000-CHOIX-VIDE-FIN.
           EXIT.
      *
       3010-CHOIX-OK-DEB.
      *
           PERFORM 6030-LECTURE-DEB
              THRU 6030-LECTURE-FIN.
           IF ART-NOTFND
              PERFORM 4010-ART-NOP-DEB
                 THRU 4010-ART-NOP-FIN
           ELSE
              PERFORM 4000-ART-OK-DEB
                 THRU 4000-ART-OK-FIN
           END-IF.
      *
       3010-CHOIX-OK-FIN.
           EXIT.
      *
       4000-ART-OK-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
           PERFORM 7100-MAP-VIDE-DEB
              THRU 7100-MAP-VIDE-FIN
           VARYING WS-N-LOT FROM 1 BY 1
             UNTIL WS-N-LOT >= 5.
      *
           PERFORM 7050-TRANSFERT-INFO-DEB
              THRU 7050-TRANSFERT-INFO-FIN.
           PERFORM 7060-TRANSFERT-INFO-TAB-DEB
              THRU 7060-TRANSFERT-INFO-TAB-FIN
           VARYING WS-N-LOT FROM 1 BY 1
             UNTIL WS-N-LOT > WS-NB-LOT.
      *
           PERFORM 7120-SAVE-MAP-DEB
              THRU 7120-SAVE-MAP-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       4000-ART-OK-FIN.
           EXIT.
      *
       4010-ART-NOP-DEB.
      *
           PERFORM 7000-AFFICHAGE-INFO-DEB
              THRU 7000-AFFICHAGE-INFO-FIN.
           PERFORM 7130-VIDAGE-MAP-DEB
              THRU 7130-VIDAGE-MAP-FIN.
           PERFORM 7100-MAP-VIDE-DEB
              THRU 7100-MAP-VIDE-FIN
           VARYING WS-N-LOT FROM 1 BY 1
             UNTIL WS-N-LOT > 5.
           PERFORM 7140-NO-CODE-DEB
              THRU 7140-NO-CODE-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       4010-ART-NOP-FIN.
           EXIT.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS PROGRAMMES         *
      *   9999-  : FIN DE PROGRAMME                                   *
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *
       6000-AFFICHAGE-FULLMAP-DEB.
           EXEC CICS SEND MAP     ('ARIM492')
                          MAPSET  ('ARIN492')
                          FROM    (ARIM492O)
                          ERASE
                          RESP    (WS-RC)
           END-EXEC.
           IF NOT CICS-OK
              MOVE 'ERREUR ENVOI MAP COMPLETE' TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-AFFICHAGE-FULLMAP-FIN.
           EXIT.
      *
       6010-AFFICHAGE-LMAP-DEB.
      *
           EXEC CICS SEND MAP    ('ARIM492')
                          MAPSET ('ARIN492')
                          FROM    (ARIM492O)
                          DATAONLY
                          ERASEAUP
                          RESP   (WS-RC)
           END-EXEC.
           IF NOT CICS-OK
              MOVE 'ERREUR ENVOI MAP LOGIQUE'  TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6010-AFFICHAGE-LMAP-FIN.
           EXIT.
      *
       6020-RECEIVE-MAP-DEB.
      *
           EXEC CICS RECEIVE
                     MAP         ('ARIM492')
                     MAPSET      ('ARIN492')
                     INTO        (ARIM492O)
                     RESP        (WS-RC)
           END-EXEC.
           IF NOT CICS-OK
               AND NOT MAPFAIL
              MOVE 'ERREUR RECEPTION MAP'      TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6020-RECEIVE-MAP-FIN.
           EXIT.
      *
       6030-LECTURE-DEB.
      *
           EXEC CICS READ
                DATASET('ART0409')
                RIDFLD (MCODEI)
                INTO (WS-ENR-SAV)
                RESP(WS-RC)
           END-EXEC.
           IF NOT CICS-OK AND NOT ART-NOTFND
              MOVE 'ERREUR LECTURE '           TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6030-LECTURE-FIN.
           EXIT.
      *
       6040-AFFICHAGE-AIDE-DEB.
      *
           EXEC CICS SEND MAP    ('ARIMHP2')
                          MAPSET ('ARIN492')
                          ERASE
                          RESP   (WS-RC)
           END-EXEC.
           IF NOT CICS-OK
              MOVE 'ERREUR MAP D''AIDE'        TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6040-AFFICHAGE-AIDE-FIN.
           EXIT.
      *
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-AFFICHAGE-INFO-DEB.
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ASKTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-ASKTIME)
                     YYYYMMDD(WS-DATE)
                     DATESEP ('/')
           END-EXEC.
      *
           MOVE LOW-VALUE              TO ARIM492O.
           MOVE EIBTRMID               TO MTERMI.
           MOVE EIBTASKN               TO MTASKI.
           MOVE EIBTRNID               TO MTRANI.
           MOVE WS-DATE                TO MDATEI.
           MOVE SPACES                 TO MMSGO.
      *
       7000-AFFICHAGE-INFO-FIN.
           EXIT.
      *
       7010-MSG-EFF-ECRAN-N-AUTOR-DEB.
      *
           MOVE WS-MSG(2)              TO  MMSGO.
      *
       7010-MSG-EFF-ECRAN-N-AUTOR-FIN.
           EXIT.
      *
       7020-MSG-ZERO-RENS-DEB.
      *
           MOVE WS-MSG(3)              TO  MMSGO.
      *
       7020-MSG-ZERO-RENS-FIN.
           EXIT.
      *
       7030-MSG-FCT-INVALIDE-DEB.
      *
           MOVE WS-MSG(1)              TO  MMSGO.
      *
       7030-MSG-FCT-INVALIDE-FIN.
           EXIT.
      *
       7040-AFFICHAGE-MAP-DEB.
      *
           SET  AFF-MAP                TO TRUE.
           MOVE SPACE                  TO MCODEI.
      *
       7040-AFFICHAGE-MAP-FIN.
           EXIT.
      *
       7050-TRANSFERT-INFO-DEB.
      *
           MOVE WS-CODE                TO MCODEI.
           MOVE WS-LIB                 TO MLIBELI.
           MOVE WS-CATEG               TO MCATEGI.
           MOVE WS-FOUR                TO MFOURI.
           MOVE WS-APPRO               TO MAPPROI.
           MOVE WS-QTE                 TO MQTSTKI.
           MOVE WS-ALERT               TO MQTALEI.
           MOVE WS-NB-LOT              TO MNLOTI.
      *
       7050-TRANSFERT-INFO-FIN.
           EXIT.
      *
       7060-TRANSFERT-INFO-TAB-DEB.
      *
           MOVE WS-TLOT-NUM(WS-N-LOT)  TO WS-LIBEL-LOT.
           MOVE WS-TLOT-QTE(WS-N-LOT)  TO WS-QTE-LOT.
           MOVE WS-TLOT-PXU(WS-N-LOT)  TO WS-PRX-LOT.
           MOVE WS-AFFICHE-LOT         TO MLOTI(WS-N-LOT).
      *
       7060-TRANSFERT-INFO-TAB-FIN.
           EXIT.
      *
       7070-RECUP-COMMAREA-DEB.
      *
           MOVE DFHCOMMAREA            TO WS-COMMAREA.
           MOVE 'A491'                 TO WS-TASK.
      *
       7070-RECUP-COMMAREA-FIN.
           EXIT.
      *
       7080-AFFICHAGE-AIDE-DEB.
      *
           SET AFF-AIDE                TO TRUE.
      *
       7080-AFFICHAGE-AIDE-FIN.
           EXIT.
      *
       7090-RETOUR-MENU-DEB.
      *
           SET INIT-TRT                TO TRUE.
           SET LOOP-MENU               TO TRUE.
           SET RESET-MAP               TO TRUE.
           MOVE LOW-VALUE              TO WS-ENR-SAV.
           MOVE WS-SPG(1)              TO WS-PROG.
      *
       7090-RETOUR-MENU-FIN.
           EXIT.
      *
       7100-MAP-VIDE-DEB.
           MOVE SPACE                  TO MLOTI(WS-N-LOT).
       7100-MAP-VIDE-FIN.
           EXIT.
      *
       7110-TAB-VIDE-DEB.
      *
           SET RESET-MAP               TO TRUE.
           MOVE SPACE                  TO MMSGO.
           MOVE WS-MSG(27)             TO MMSGO.
           MOVE SPACE                  TO MLIBELI
                                          MCATEGI
                                          MFOURI
                                          MAPPROI
                                          MQTSTKI
                                          MQTALEI
                                          MNLOTI.
      *
       7110-TAB-VIDE-FIN.
           EXIT.
      *
       7120-SAVE-MAP-DEB.
      *
           SET SAVE-MAP                TO TRUE.
           MOVE SPACE                  TO MMSGO.
      *
       7120-SAVE-MAP-FIN.
           EXIT.
      *
       7130-VIDAGE-MAP-DEB.
      *
           MOVE SPACE                  TO MCODEI
                                          MLIBELI
                                          MCATEGI
                                          MFOURI
                                          MAPPROI
                                          MNLOTI
                                          MQTSTKI
                                          MQTALEI.
      *
       7130-VIDAGE-MAP-FIN.
           EXIT.
      *
       7140-NO-CODE-DEB.
      *
           SET RESET-MAP               TO TRUE.
           MOVE SPACE                  TO MCODEI
                                          MLIBELI
                                          MCATEGI
                                          MFOURI
                                          MAPPROI
                                          MNLOTI
                                          MQTSTKI
                                          MQTALEI.
           MOVE SPACE                  TO MMSGO.
           MOVE WS-MSG(27)             TO MMSGO.
      *
       7140-NO-CODE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8999-STATISTIQUES-DEB.
      *
      *8999-STATISTIQUES-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT APPEL-SPG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE LORS DE CHAQUE APPEL D'UN SOUS PROGRAMME    *
      * (APRES SAISIE D'UN CHOIX OU A CHAQUE BOUCLE SUR UN DES        *
      * TRAITEMENTS DEPENDANTS)                                       *
      * IL PERMET :                                                   *
      * ==> D'INITIALISER LE NOM DU SOUS PROGRAMME A APPELER          *
      * ==> DE DONNER DYNAMIQUEMENT LE CONTROLE PROGRAMME             *
      *     CORRESPONDANT EN LUI TRANSMETTANT UNE COMMAREA QUI PERMET *
      *     DE SAUVEGARDER LES DONNEES NECCESSAIRES A LA POURSUITE    *
      *     DU TRAITEMENT (PROGRAMMATION PSEUDO CONVERSATIONNELLE)    *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SPG-DEB.
      *    MOVE WS-SPG(WS-AIG) TO WS-PROG.
      *    EXEC CICS XCTL PROGRAM(WS-PROG)
      *                   COMMAREA(WS-COMMAREA)
      *                   RESP(WS-RC)
      *    END-EXEC.
      *9000-APPEL-SPG-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : FIN DE PROGRAMME                                   *
      *---------------------------------------------------------------*
      *
       9000-MENU-DEB.
      *
           EXEC CICS XCTL
                PROGRAM   (WS-PROG)
                COMMAREA  (WS-COMMAREA)
                RESP      (WS-RC)
           END-EXEC.
      *
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE 'ERREUR APPEL MENU'    TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       9000-MENU-FIN.
           EXIT.
      *
       9999-FIN-PROGRAMME-DEB.
           MOVE WS-MSG(26)                TO WS-MSG-FIN.
           EXEC CICS SEND
                     FROM (WS-MSG-FIN)
                     ERASE
                     RESP (WS-RC)
           END-EXEC.
           EXEC CICS RETURN
                     RESP (WS-RC)
           END-EXEC.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT FIN-RTRANSID           *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE APRES CHAQUE AFFICHAGE POUR TERMINER LA     *
      * TRANSACTION DE FACON TEMPORAIRE.                              *
      * L'OPTION TRANSID INDIQUE LE CODE TRANSACTION QUI SERA UTILISE *
      * PAR CICS POUR REINITIALISER LA TRANSACTION (EIBTRNID CONTIENT *
      * LE DERNIER CODE UTILISE).                                     *
      * L'OPTION COMMAREA PERMET DE TRANSMETTRE UNE ZONE QUI PERMET   *
      * SAUVEGARDER DES DONNEES QUI SERONT RECUPEREES PAR LE PROGRAMME*
      * POUR LE COMPTE DE LA TRANSACTION QUI SERA REACTIVEE.          *
      *---------------------------------------------------------------*
      *
       9999-FIN-RETURN-TRANSID-DEB.
           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(WS-COMMAREA)
                     RESP(WS-RC)
           END-EXEC.
       9999-FIN-RETURN-TRANSID-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
      * MESSAGE D ERREUR
           EXEC CICS SEND
                     FROM(WS-MSG-FIN)
                     ERASE
                     RESP(WS-RC)
           END-EXEC.
      *
      * FIN DU PROGRAMME
           EXEC CICS RETURN
                     RESP(WS-RC)
           END-EXEC.
      *
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ABEND-PRG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE QUAND UNE ERREUR EST DETECTEE LORS DU       *
      * TEST SUR LE CONTEXTE D'EXECUTION (CODE DIFFERENT D'UNE BOUCLE *
      * SUR LA GESTION DU MENU OU SUR UN DES TRAITEMENTS DEPENDANTS). *
      *                                                               *
      *                           ATTENTION !                         *
      * AUCUN CODE (ABCODE) N'EST UTILISE POUR IDENTIFIE L'ABEND      *
      * (UNE SEULE CONDITION D'ABEND) ET L'OPTION NODUMP PERMET DE    *
      * SUPPRIMER L'IMPRESSION PAR DEFAUT D'UN DUMP.                  *
      *---------------------------------------------------------------*
      *
       9999-ABEND-PRG-DEB.
            EXEC CICS ABEND
                      NODUMP
            END-EXEC.
       9999-ABEND-PRG-FIN.
           EXIT.
