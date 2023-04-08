      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC491                                   *
      *  NOM DU REDACTEUR : PHAN CHARLIE                              *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 22/03/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * PROGRAMME DE GESTION D'UN MENU                                *
      * MET EN OEUVRE LES ORDRES D'AFFICHAGE ET DE LECTURE DES MAPS   *
      *               L'ORDRE D'AFFICHAGE DES DONNEES NON FORMATE     *
      *               LES ORDRES DE GESTION DES PROGRAMMES.           *
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
       PROGRAM-ID. ARIC491
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
       01  WS-MCHOIXT                    PIC X.
       77  WS-MSG-FIN                    PIC X(79).
       01  WS-PROG                       PIC X(8).
      *
       01  WS-TABSPG.
           05  FILLER                    PIC X(8)   VALUE 'ARIC492'.
           05  FILLER                    PIC X(8)   VALUE 'ARIC493'.
           05  FILLER                    PIC X(8)   VALUE 'ARIC494'.
           05  FILLER                    PIC X(8)   VALUE 'ARIC495'.
           05  FILLER                    PIC X(8)   VALUE 'ARIC496'.
           05  FILLER                    PIC X(8)   VALUE 'ARIC497'.
       01  FILLER REDEFINES WS-TABSPG.
           05  WS-SPG                    PIC X(8) OCCURS 6.
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
           COPY ARIN491.
      * VARIABLE COMMAREA
           COPY COMMAREA.
      *
      *================
       LINKAGE SECTION.
      *================
      *
       01  DFHCOMMAREA                  PIC X(4096).
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
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
       0000-PROGRAMME-DEB.
      *
           PERFORM 7040-INIT-DATE-DEB
              THRU 7040-INIT-DATE-FIN.
           IF EIBCALEN = 0
              PERFORM 1000-PRE-AFFICHAGE-DEB
                 THRU 1000-PRE-AFFICHAGE-FIN
           ELSE
              PERFORM 1010-N-AFFICHAGE-DEB
                 THRU 1010-N-AFFICHAGE-FIN
           END-IF.
      *
           PERFORM 9999-FIN-RETURN-TRANSID-DEB
              THRU 9999-FIN-RETURN-TRANSID-FIN.
      *
       0000-PROGRAMME-FIN.
            STOP RUN.
      *
       1000-PRE-AFFICHAGE-DEB.
      *
           SET LOOP-MENU                       TO TRUE.
           PERFORM 7050-INIT-INFO-DEB
              THRU 7050-INIT-INFO-FIN.
           PERFORM 6000-AFFICHAGE-FULLMAP-DEB
              THRU 6000-AFFICHAGE-FULLMAP-FIN.
      *
       1000-PRE-AFFICHAGE-FIN.
           EXIT.
      *
       1010-N-AFFICHAGE-DEB.
           PERFORM 7080-SELECT-PGM-DEB
              THRU 7080-SELECT-PGM-FIN.
      *
           EVALUATE TRUE
               WHEN LOOP-MENU
                    PERFORM 2000-MENU-DEB
                       THRU 2000-MENU-FIN
               WHEN LOOP-SPG
                    PERFORM 2010-SOUS-PGM-DEB
                       THRU 2010-SOUS-PGM-FIN
               WHEN OTHER
                    PERFORM 2020-AUTRE-DEB
                       THRU 2020-AUTRE-FIN
           END-EVALUATE.
      *
       1010-N-AFFICHAGE-FIN.
           EXIT.
      *
       2000-MENU-DEB.
      *
           EVALUATE EIBAID
      *
               WHEN DFHENTER
                    PERFORM 3000-VALIDATION-DEB
                       THRU 3000-VALIDATION-FIN
               WHEN DFHPF3
                    PERFORM 3010-RETOUR-DEB
                       THRU 3010-RETOUR-FIN
               WHEN DFHCLEAR
                    PERFORM 3020-EFFACEMENT-ECRAN-DEB
                       THRU 3020-EFFACEMENT-ECRAN-FIN
               WHEN OTHER
                    PERFORM 3030-ERREUR-TOUCHE-DEB
                       THRU 3030-ERREUR-TOUCHE-FIN
           END-EVALUATE.
      *
       2000-MENU-FIN.
           EXIT.
      *
       2010-SOUS-PGM-DEB.
      *
           PERFORM 9000-APPEL-SPG-DEB
              THRU 9000-APPEL-SPG-FIN.
      *
       2010-SOUS-PGM-FIN.
           EXIT.
      *
       2020-AUTRE-DEB.
      *
           PERFORM 7030-MSG-CHOIX-INVALIDE-DEB
              THRU 7030-MSG-CHOIX-INVALIDE-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
           PERFORM 9999-ABEND-PRG-DEB
              THRU 9999-ABEND-PRG-FIN.
      *
       2020-AUTRE-FIN.
           EXIT.
      *
       3000-VALIDATION-DEB.
      *
      * OREILLETTE GAUCHE
           PERFORM 6020-RECEIVE-MAP-DEB
              THRU 6020-RECEIVE-MAP-FIN.
      *
           IF WS-RC = DFHRESP(MAPFAIL) OR MCHOIXO = SPACE
              PERFORM 4000-CHOIX-VIDE-DEB
                 THRU 4000-CHOIX-VIDE-FIN
           ELSE
              PERFORM 4010-CHOIX-OK-DEB
                 THRU 4010-CHOIX-OK-FIN
           END-IF.
      *
       3000-VALIDATION-FIN.
           EXIT.
      *
       3010-RETOUR-DEB.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       3010-RETOUR-FIN.
           EXIT.
      *
       3020-EFFACEMENT-ECRAN-DEB.
      *
           PERFORM 7050-INIT-INFO-DEB
              THRU 7050-INIT-INFO-FIN.
           PERFORM 7010-MSG-EFF-ECRAN-N-AUTOR-DEB
              THRU 7010-MSG-EFF-ECRAN-N-AUTOR-FIN.
      *
           PERFORM 6000-AFFICHAGE-FULLMAP-DEB
              THRU 6000-AFFICHAGE-FULLMAP-FIN.
      *
       3020-EFFACEMENT-ECRAN-FIN.
           EXIT.
      *
       3030-ERREUR-TOUCHE-DEB.
      *
           PERFORM 7050-INIT-INFO-DEB
              THRU 7050-INIT-INFO-FIN.
           PERFORM 7000-MSG-FCT-INVALIDE-DEB
              THRU 7000-MSG-FCT-INVALIDE-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       3030-ERREUR-TOUCHE-FIN.
           EXIT.
      *
       4000-CHOIX-VIDE-DEB.
      *
           PERFORM 7050-INIT-INFO-DEB
              THRU 7050-INIT-INFO-FIN.
           PERFORM 7020-MSG-ZERO-RENS-DEB
              THRU 7020-MSG-ZERO-RENS-FIN.
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       4000-CHOIX-VIDE-FIN.
           EXIT.
      *
       4010-CHOIX-OK-DEB.
      *
           PERFORM 7070-SAVE-CHOIX-DEB
              THRU 7070-SAVE-CHOIX-FIN.
           IF CHOIX-OK
              PERFORM 5010-OPTION-OK-DEB
                 THRU 5010-OPTION-OK-FIN
           ELSE
              PERFORM 5000-ERREUR-OPTION-DEB
                 THRU 5000-ERREUR-OPTION-FIN
           END-IF.
      *
       4010-CHOIX-OK-FIN.
           EXIT.
      *
       5000-ERREUR-OPTION-DEB.
      *
           PERFORM 7050-INIT-INFO-DEB
              THRU 7050-INIT-INFO-FIN.
           PERFORM 7030-MSG-CHOIX-INVALIDE-DEB
              THRU 7030-MSG-CHOIX-INVALIDE-FIN.
      *
           PERFORM 6010-AFFICHAGE-LMAP-DEB
              THRU 6010-AFFICHAGE-LMAP-FIN.
      *
       5000-ERREUR-OPTION-FIN.
           EXIT.
      *
       5010-OPTION-OK-DEB.
      *
           PERFORM 9000-APPEL-SPG-DEB
              THRU 9000-APPEL-SPG-FIN.
      *
       5010-OPTION-OK-FIN.
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
           EXEC CICS SEND MAP     ('ARIM491')
                          MAPSET  ('ARIN491')
                          ERASE
                          RESP    (WS-RC)
           END-EXEC.
           IF WS-RC NOT = DFHRESP(NORMAL)
              MOVE 'ERREUR ENVOI MAP COMPLETE' TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-AFFICHAGE-FULLMAP-FIN.
           EXIT.
      *
       6010-AFFICHAGE-LMAP-DEB.
      *
           EXEC CICS SEND MAP    ('ARIM491')
                          MAPSET ('ARIN491')
                          DATAONLY
                          ERASEAUP
                          RESP   (WS-RC)
           END-EXEC.
           IF WS-RC NOT = DFHRESP(NORMAL)
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
                     MAP         ('ARIM491')
                     MAPSET      ('ARIN491')
                     INTO        (ARIM491O)
                     RESP        (WS-RC)
           END-EXEC.
           IF NOT (WS-RC = DFHRESP(NORMAL)
               OR WS-RC = DFHRESP(MAPFAIL))
              MOVE 'ERREUR RECEPTION MAP'      TO WS-MSG-FIN
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6020-RECEIVE-MAP-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-MSG-FCT-INVALIDE-DEB.
      *
           MOVE WS-MSG(1)      TO  MMSGO.
      *
       7000-MSG-FCT-INVALIDE-FIN.
           EXIT.
      *
       7010-MSG-EFF-ECRAN-N-AUTOR-DEB.
      *
           MOVE WS-MSG(2)      TO  MMSGO.
      *
       7010-MSG-EFF-ECRAN-N-AUTOR-FIN.
           EXIT.
      *
       7020-MSG-ZERO-RENS-DEB.
      *
           MOVE WS-MSG(24)     TO  MMSGO.
      *
       7020-MSG-ZERO-RENS-FIN.
           EXIT.
      *
       7030-MSG-CHOIX-INVALIDE-DEB.
      *
           MOVE WS-MSG(25)     TO  MMSGO.
      *
       7030-MSG-CHOIX-INVALIDE-FIN.
           EXIT.
      *
       7040-INIT-DATE-DEB.
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ASKTIME)
           END-EXEC.
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ASKTIME)
                     YYYYMMDD(WS-DATE)
                     DATESEP
           END-EXEC.
      *
       7040-INIT-DATE-FIN.
           EXIT.
      *
       7050-INIT-INFO-DEB.
      *
           MOVE LOW-VALUE      TO ARIM491O.
           MOVE EIBTRMID       TO MTERMO.
           MOVE EIBTASKN       TO MTASKO.
           MOVE EIBTRNID       TO MTRANO.
           MOVE WS-DATE        TO MDATEO.
           MOVE SPACES         TO MMSGO.
      *
       7050-INIT-INFO-FIN.
           EXIT.
      *
       7060-PREP-MSG-SELECTION-DEB.
      *
           STRING 'OPTION N° '     DELIMITED BY SIZE
                  WS-MCHOIXT       DELIMITED BY SIZE
                  ' SELECTIONNÉ.'  DELIMITED BY SIZE
             INTO MMSGO
           END-STRING.
      *
       7060-PREP-MSG-SELECTION-FIN.
           EXIT.
      *
       7070-SAVE-CHOIX-DEB.
      *
           MOVE MCHOIXO         TO WS-MCHOIXT.
      *
       7070-SAVE-CHOIX-FIN.
           EXIT.
      *
       7080-SELECT-PGM-DEB.
      *
           SET INIT-TRT         TO TRUE.
           MOVE MCHOIXO         TO WS-AIG.
           ADD 1                TO WS-AIG.
      *     MOVE 'ARIC49'        TO WS-SPG.
           MOVE WS-SPG(WS-AIG)  TO WS-PROG.
      *
       7080-SELECT-PGM-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
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
       9000-APPEL-SPG-DEB.
           EXEC CICS XCTL PROGRAM(WS-PROG)
      *     EXEC CICS XCTL PROGRAM('ARIC492')
                          COMMAREA(WS-COMMAREA)
                          RESP(WS-RC)
           END-EXEC.
       9000-APPEL-SPG-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : FIN DE PROGRAMME                                   *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
           MOVE WS-MSG(26) TO WS-MSG-FIN.
           EXEC CICS SEND
                     FROM (WS-MSG-FIN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN
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
