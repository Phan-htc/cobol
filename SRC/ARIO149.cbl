      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO149                                   *
      *  NOM DU REDACTEUR : PHAN                                      *
      *  SOCIETE          : TRICOPHO                                  *
      *  DATE DE CREATION : --/--/----                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *DECRIT DES OPERATIONS BANCAIRE, EDITER UN ETAT MENSUEL PAR UN  *
      *COMPTE CLIENT.                                                 *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 03/01/2023    !      CREATION DU PROGRAMME                    *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO149.
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
      *                      F-MVTS-E: FICHIER DES MOUVEMENTS (MVTS)
      *                      -------------------------------------------
           SELECT  F-MVTS-E             ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
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
           RECORD CONTAINS 50 CHARACTERS.
      *
       01  FS-ENRG-F-MVTS              PIC X(50).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
       77 WS-FS-MVTS-E                 PIC XX.
       01 WS-ENRG-F-MVTS.
           05 WS-MVTS-CPTE             PIC 9(10).
           05 WS-MVTS-DATE.
                10 WS-MVTS-ANNEE.
                   15 WS-MVTS-SS       PIC 99.
                   15 WS-MVTS-AA       PIC 99.
                10 WS-MVTS-MM          PIC 99.
                10 WS-MVTS-JJ          PIC 99.
           05 WS-MVTS-CODE             PIC X.
           05 WS-MVTS-MT               PIC 9(8)V99.
           05 FILLER                   PIC X(21).
      *
       01 WS-LASTER.
          05 FILLER                    PIC X(45)
                                       VALUE ALL '*'.
      *
       01 WS-LCPTE.
          05 FILLER                    PIC X(16)
                                       VALUE 'NUMERO DE COMPTE'.
          05 FILLER                    PIC X(11)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-OCPT                   PIC 9(10).
      *
       01 WS-LTIRET.
          05 FILLER                    PIC X(45)
                                       VALUE ALL '-'.
      *
       01 WS-LCB.
          05 FILLER                    PIC X(17)
                                       VALUE 'CUMUL CARTE-BLEUE'.
          05 FILLER                    PIC X(10)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-OCB                    PIC 9(10)V99.
      *
       01 WS-LRDAB.
          05 FILLER                    PIC X(17)
                                       VALUE 'CUMUL RETRAIT DAB'.
          05 FILLER                    PIC X(10)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-ORDAB                  PIC 9(10)V99.
      *
       01 WS-LDGUI.
          05 FILLER                    PIC X(19)
                                       VALUE 'CUMUL DEPOT GUICHET'.
          05 FILLER                    PIC X(8)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-ODGUI                  PIC 9(10)V99.
      *
       01 WS-LBAL.
          05 FILLER                    PIC X(22)
                                       VALUE 'BALANCE DES OPERATIONS'.
          05 FILLER                    PIC X(5)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-OBAL                   PIC S9(11)V99.
      *
       01 WS-LECPT.
          05 FILLER                    PIC X(21)
                                       VALUE 'ERREUR POUR LE COMPTE'.
          05 FILLER                    PIC X(6)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-ECPT                   PIC 9(10).
      *
       01 WS-LEMVT.
          05 FILLER                    PIC X(14)
                                       VALUE 'CODE MOUVEMENT'.
          05 FILLER                    PIC X(13)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-EMVT                   PIC X.
      *
       01 WS-LEMT.
          05 FILLER                    PIC X(7)
                                       VALUE 'MONTANT'.
          05 FILLER                    PIC X(20)
                                       VALUE SPACE.
          05 FILLER                    PIC X
                                       VALUE ':'.
          05 FILLER                    PIC X(3)
                                       VALUE SPACE.
          05 WS-EMT                    PIC 9(10)V99.
      *
       01 WS-CCLI                      PIC 9(3)
                                       VALUE 0.
       01 WS-CMVT                      PIC 9(3)
                                       VALUE 0.
       01 WS-CERR                      PIC 9(3)
                                       VALUE 0.
       01 WS-CRET                      PIC 9(3)
                                       VALUE 0.
       01 WS-CCB                       PIC 9(3)
                                       VALUE 0.
       01 WS-CDEP                      PIC 9(3)
                                       VALUE 0.
       01 WS-CPT-MVT                   PIC 9(3)
                                       VALUE 0.

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
      *                          XXYY-COMPOSANR-FIN                   *
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
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
       0000-TRT-PRINCIPAL-DEB.
      *PREPARATION DU TRAITEMENT                                      *
           PERFORM  6000-OPEN-MVTS-DEB
              THRU  6000-OPEN-MVTS-FIN.
      *
           PERFORM  6010-READ-MVTS-DEB
              THRU  6010-READ-MVTS-FIN.
      *
                    IF WS-FS-MVTS-E = '10'
                        THEN PERFORM  8000-EOF-MVTS-DEB
                                THRU  8000-EOF-MVTS-FIN
                    END-IF.
      *                                                               *
      *APPEL DU COMPOSANT SUIVANT (ITERATIVE)                         *
           PERFORM  1000-TRT-COMPTE-DEB
              THRU  1000-TRT-COMPTE-FIN
           UNTIL  WS-FS-MVTS-E = '10'.
      *                                                               *
      *FIN DU TRAITEMENT                                              *
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN.
           PERFORM  8030-ECRIRE-EXECUTION-DEB
              THRU  8030-ECRIRE-EXECUTION-FIN.
           PERFORM  6030-CLOSE-MVTS-DEB
              THRU  6030-CLOSE-MVTS-FIN.
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
            STOP RUN.
       1000-TRT-COMPTE-DEB.
      * PREPARATION DU TRAITEMENT                                     *
      * INITIALISER LES VARIABLES DU COMPTE A TRAITER                 *
      *                                                               *
           PERFORM  7070-INIT-DEB
              THRU  7070-INIT-FIN.
      *
      * ITERATIVE (CONTINU LE TRAITEMENT TANT QUE FS != 10)           *
      *                                                               *
           PERFORM 2000-TRT-MOUVEMENT-DEB
              THRU 2000-TRT-MOUVEMENT-FIN
             UNTIL WS-MVTS-CPTE NOT = WS-OCPT      OR
                    WS-FS-MVTS-E = '10'.

      *                                                               *
      *FIN DU TRAITEMENT                                              *
      *COMPTE LE CLIENT, SI MVT VALIDE ECRIRE ETAT OPERATION          *
      *
           PERFORM 7060-COMPTER-CLIENT-DEB
              THRU 7060-COMPTER-CLIENT-FIN.
           IF WS-ODGUI          NOT = 0            OR
              WS-OCB            NOT = 0            OR
              WS-ORDAB          NOT = 0
                   PERFORM 7050-BALANCE-DEB
                      THRU 7050-BALANCE-FIN
                   PERFORM 8010-ECRIRE-OPE-DEB
                      THRU 8010-ECRIRE-OPE-FIN
           END-IF.
       1000-TRT-COMPTE-FIN.
           EXIT.
      *                                                               *
       2000-TRT-MOUVEMENT-DEB.
      *                                                               *
      *AUCUNE PREPARATION                                             *
      *                                                               *
      *QUEL EST LE CODE DU MOUVEMENT ?                                *
      *                                                               *
           EVALUATE WS-MVTS-CODE
               WHEN 'C'
                           PERFORM 3000-TRT-CARTE-DEB
                              THRU 3000-TRT-CARTE-FIN
               WHEN 'R'
                           PERFORM 3010-TRT-RETRAIT-DEB
                              THRU 3010-TRT-RETRAIT-FIN
               WHEN 'D'
                           PERFORM 3020-TRT-DEPOT-DEB
                              THRU 3020-TRT-DEPOT-FIN
               WHEN OTHER  PERFORM 3030-TRT-ERREUR-DEB
                              THRU 3030-TRT-ERREUR-FIN
           END-EVALUATE.
      *FIN DU TRAITEMENT MOUVEMENT                                    *
      *ON FAIT LES COMPTES                                            *
           PERFORM 6010-READ-MVTS-DEB
              THRU 6010-READ-MVTS-FIN.
       2000-TRT-MOUVEMENT-FIN.
           EXIT.
      *
       3000-TRT-CARTE-DEB.
           PERFORM 7010-CARTE-DEB
              THRU 7010-CARTE-FIN.
       3000-TRT-CARTE-FIN.
           EXIT.
      *
       3010-TRT-RETRAIT-DEB.
           PERFORM 7020-RETRAIT-DEB
              THRU 7020-RETRAIT-FIN.
       3010-TRT-RETRAIT-FIN.
           EXIT.
      *
       3020-TRT-DEPOT-DEB.
           PERFORM 7030-DEPOT-DEB
              THRU 7030-DEPOT-FIN.
       3020-TRT-DEPOT-FIN.
           EXIT.
      *
       3030-TRT-ERREUR-DEB.
           PERFORM 7040-ERREUR-DEB
              THRU 7040-ERREUR-FIN.
           PERFORM 8020-ECRIRE-ERREUR-DEB
              THRU 8020-ECRIRE-ERREUR-FIN.
       3030-TRT-ERREUR-FIN.
           EXIT.
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
       6000-OPEN-MVTS-DEB.
           OPEN INPUT F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
               DISPLAY 'FICHIER F-MVTS-E ERREUR'
               DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-MVTS-FIN.
           EXIT.
       6010-READ-MVTS-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (WS-FS-MVTS-E = '00' OR '10')
               DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
               DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-READ-MVTS-FIN.
      *                                                       *
           EXIT.
       6030-CLOSE-MVTS-DEB.
           CLOSE F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
               DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
               DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
        6030-CLOSE-MVTS-FIN.
           EXIT.
      *6000-ORDRE-FICHIER-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
       7010-CARTE-DEB.
           ADD WS-MVTS-MT     TO WS-OCB.
           ADD 1              TO WS-CMVT.
           ADD 1              TO WS-CCB.
           ADD 1              TO WS-CPT-MVT.
       7010-CARTE-FIN.
      *                                                               *
           EXIT.
       7020-RETRAIT-DEB.
           ADD WS-MVTS-MT     TO WS-ORDAB.
           ADD 1              TO WS-CMVT.
           ADD 1              TO WS-CRET.
           ADD 1              TO WS-CPT-MVT.
       7020-RETRAIT-FIN.
           EXIT.
      *                                                               *
       7030-DEPOT-DEB.
           ADD WS-MVTS-MT     TO WS-ODGUI.
           ADD 1              TO WS-CMVT.
           ADD 1              TO WS-CDEP.
           ADD 1              TO WS-CPT-MVT.
       7030-DEPOT-FIN.
           EXIT.
      *                                                               *
       7040-ERREUR-DEB.
           ADD 1              TO WS-CERR.
           ADD 1              TO WS-CMVT.
           MOVE WS-MVTS-CPTE  TO WS-ECPT.
           MOVE WS-MVTS-CODE  TO WS-EMVT.
           MOVE WS-MVTS-MT    TO WS-EMT.
       7040-ERREUR-FIN.
      *
           EXIT.
       7050-BALANCE-DEB.
           COMPUTE WS-OBAL = WS-ODGUI - WS-OCB - WS-ORDAB.
       7050-BALANCE-FIN.
           EXIT.
       7060-COMPTER-CLIENT-DEB.
           ADD 1              TO WS-CCLI.
       7060-COMPTER-CLIENT-FIN.
           EXIT.
       7070-INIT-DEB.
           MOVE WS-MVTS-CPTE  TO WS-OCPT.
           MOVE ZERO          TO WS-OCB.
           MOVE ZERO          TO WS-ORDAB.
           MOVE ZERO          TO WS-ODGUI.
           MOVE ZERO          TO WS-CPT-MVT.
       7070-INIT-FIN.
           EXIT.
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
       8000-EOF-MVTS-DEB.
           DISPLAY 'FICHIER F-MVTS-E VIDE'.
       8000-EOF-MVTS-FIN.
           EXIT.
      *AFFICHE L ETAT DES OPERATIONS                                 *
       8010-ECRIRE-OPE-DEB.
           DISPLAY WS-LASTER.
           DISPLAY WS-LCPTE.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LCB.
           DISPLAY WS-LRDAB.
           DISPLAY WS-LDGUI.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LBAL.
           DISPLAY WS-LASTER.
       8010-ECRIRE-OPE-FIN.
           EXIT.
      *AFFICHE L ETAT DES ERREURS                                     *
       8020-ECRIRE-ERREUR-DEB.
           DISPLAY WS-LASTER.
           DISPLAY WS-LECPT.
           DISPLAY WS-LEMVT.
           DISPLAY WS-LEMT.
           DISPLAY WS-LASTER.
       8020-ECRIRE-ERREUR-FIN.
           EXIT.
      *AFFICHE LE COMPTE RENDU D EXECUTION                            *
       8030-ECRIRE-EXECUTION-DEB.
           DISPLAY WS-LASTER.
           DISPLAY 'NOMBRE DE CLIENTS            :   ',
                     WS-CCLI
           DISPLAY 'NOMBRE DE MOUVEMENTS         :   ',
                     WS-CMVT
           DISPLAY 'NOMBRE DE MOUVEMENTS ERRONES :   ',
                     WS-CERR
           DISPLAY 'NOMBRE DE RETRAITS           :   ',
                     WS-CRET
           DISPLAY 'NOMBRE CARTES BLEUES         :   ',
                     WS-CCB
           DISPLAY 'NOMBRE DE DEPOTS             :   ',
                     WS-CDEP
           DISPLAY WS-LASTER.
       8030-ECRIRE-EXECUTION-FIN.
           EXIT.
      *8000-ORDRE-EDITION-FIN.
      *
       8999-STATISTIQUES-DEB.
      *
           DISPLAY '************************************************'
           DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO149        *'
           DISPLAY '*     ==================================       *'
           DISPLAY '************************************************'.
      *
       8999-STATISTIQUES-FIN.
           EXIT.
      *
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
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO149         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO149        *'.
           DISPLAY '*==============================================*'.
           MOVE 12            TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
