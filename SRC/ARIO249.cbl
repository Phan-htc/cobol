      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO249                                   *
      *  NOM DU REDACTEUR : PHAN                                      *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 06/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * EDITER UN ETAT BIMENSUEL DES OPERATIONS BANCAIRES             *
      *                                                               *
      * EDITER UN FICHIER ANOMALIE EN CAS DE CODE ERREUR AUTRE QUE    *
      * C POUR CARTE BLEUE, D POUR DEPOT EN GUICHET ET R POUR RETRAIT *
      *
      * EDITER UN FICHIER CLIENT AVEC :                               *
      *  - UN DETAIL DE CHAQUE OPERATION CONTENANT LE LIBELLE DE      *
      *     L'OPERATION, LE MONTANT DU DEBIT OU LE MONTANT DU CREDIT. *
      *  - UN TOTAL ADDITIONNANT LES DEBITS ET LES CREDITS.           *
      *                                                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 06/02/2023    !     CREATION DU PROGRAMME                     *
      *               !---------------------------------              *
      * 09/02/2023    !     FIN DE CREATION DU PROGRAMME              *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO249.
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
      *                      MVTS : FICHIER DES MOUVEMENTS
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER ETAT CLIENT
      *                      -------------------------------------------
           SELECT F-ETATCLI-S          ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETATCLI-S.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATANO : FICHIER ETAT DES ANOMALIES
      *                      -------------------------------------------
           SELECT F-ETATANO-S          ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETATANO-S.
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
       01  FS-F-MVTS-E                       PIC X(50).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI-S                 PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO-S                 PIC X(80).
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      * DECLARATION DES VARIABLES DES FICHIERS
      *
       77  WS-FS-MVTS-E                      PIC X(2).
      *
       77  WS-FS-ETATCLI-S                   PIC X(2).
      *
       77  WS-FS-ETATANO-S                   PIC X(2).
      *
       01  WS-MVTS-E.
           05  WS-MVTS-CPTE                  PIC 9(10).
           05  WS-MVTS-DATE.
               10  WS-MVTS-ANNEE.
                   15  WS-MVTS-SS            PIC 99.
                   15  WS-MVTS-AA            PIC 99.
               10  WS-MVTS-MM                PIC 99.
               10  WS-MVTS-JJ                PIC 99.
           05  WS-MVTS-CODE                  PIC X.
           05  WS-MVTS-MT                    PIC 9(8)V99.
           05  FILLER                        PIC X(21).
      *
       01  WS-ENRG-F-ETATCLI-S.
      *
           05  WS-LETAT-ASTER                PIC X(78)        VALUE
               ALL '*'.
      *
           05  WS-LETAT-ENT.
               10  FILLER                    PIC X(2)         VALUE
                   '*'.
               10  FILLER                    PIC X(19)        VALUE
                   'NUMERO DE COMPTE: '.
               10  WS-LETAT-AST-CPTE-ED      PIC 9(10).
               10  FILLER                    PIC X(34)        VALUE
                   SPACE.
               10  FILLER                    PIC X(2)         VALUE
                   'LE'.
               10  FILLER                    PIC X            VALUE
                   SPACE.
               10  WS-LETAT-AST-DATE-ED.
                   15  WS-JOUR               PIC 99.
                   15  FILLER                PIC X            VALUE
                   '/'.
                   15  WS-MOIS               PIC 99.
                   15  FILLER                PIC X            VALUE
                   '/'.
                   15  WS-ANNEE              PIC 99.
               10  FILLER                    PIC X(2)         VALUE
                   ' *'.
      *
           05  WS-LETAT-TITRE.
               10  FILLER                    PIC X(10)        VALUE
                   '* LIBELLE'.
               10  FILLER                    PIC X(37)        VALUE
                   SPACE.
               10  FILLER                    PIC X(15)        VALUE
                   '*     DEBIT   '.
               10  FILLER                    PIC X(16)        VALUE
                   '*     CREDIT   *'.
      *
           05  WS-LETAT-DETAIL.
               10  FILLER                    PIC X(2)         VALUE
                   '*'.
               10  WS-LETAT-DET-MVT-ED       PIC X(45).
               10  FILLER                    PIC X            VALUE
                   '*'.
               10  WS-LETAT-DET-MTDB-ED      PIC Z(9)9,99
                   BLANK WHEN ZERO.
               10  FILLER                    PIC X(2)         VALUE
                   ' *'.
               10  WS-LETAT-DET-MTCR-ED      PIC Z(9)9,99
                   BLANK WHEN ZERO.
               10  FILLER                    PIC X(2)         VALUE
                   ' *'.
      *
           05  WS-LETAT-TOTAL.
               10  FILLER                    PIC X(7)         VALUE
                   '* TOTAL'.
               10  FILLER                    PIC X(40)        VALUE
                   SPACE.
               10  FILLER                    PIC X            VALUE
                   '*'.
               10  WS-LETAT-TOT-MTDB-ED      PIC Z(9)9,99
                   BLANK WHEN ZERO.
               10  FILLER                    PIC X(2)         VALUE
                   ' *'.
               10  WS-LETAT-TOT-MTCR-ED      PIC Z(9)9,99
                   BLANK WHEN ZERO.
               10  FILLER                    PIC X(2)         VALUE
                   ' *'.
      *
       01  WS-LANO-L1.
           05  FILLER                        PIC X            VALUE
              '*'.
           05  FILLER                        PIC X(55)        VALUE
               ALL '-'.
           05  FILLER                        PIC X            VALUE
               '*'.
      *
       01  WS-LANO-TITRE.
           05  FILLER                        PIC X(34)        VALUE
               '|  NO COMPTE   |  CODE MOUVEMENT |'.
           05  FILLER                        PIC X(23)        VALUE
               '     MONTANT          |'.
      *
       01  WS-LANO-L3.
           05  FILLER                        PIC X            VALUE
               '|'.
           05  FILLER                        PIC X(55)        VALUE
               ALL '-'.
           05  FILLER                        PIC X            VALUE
               '|'.
      *
       01  WS-LANO-DETAIL.
           05  FILLER                        PIC X(3)         VALUE
               '|  '.
           05  WS-LANO-DET-CPT-ED            PIC 9(10).
           05  FILLER                        PIC X(3)         VALUE
               '  |'.
           05  FILLER                        PIC X(7)         VALUE
               ALL SPACE.
           05  WS-LANO-DET-MVT-ED            PIC X.
           05  FILLER                        PIC X(18)        VALUE
               '         |     '.
           05  WS-LANO-DET-MT-ED             PIC Z(7)9,99.
           05  FILLER                        PIC X(4)         VALUE
               '   |'.
      *
       01  WS-LANO-TOTAL.
           05  FILLER                        PIC X(30)        VALUE
               '| MONTANT TOTAL DES ANOMALIES'.
           05  FILLER                        PIC X(10)        VALUE
               '   |'.
           05  WS-LANO-TOT-MT-ED             PIC Z(9)9,99.
           05  FILLER                        PIC X(4)         VALUE
               '   |'.
       01  WS-LCRE-ASTER                     PIC X(45)        VALUE
               ALL '*'.
       01  WS-LCRE-TITRE                     PIC X(45)        VALUE
                   '*     COMPTE RENDU D''EXECUTION (ARIO249)'.
       01  WS-LCRE-DETAIL.
           05  FILLER                        PIC X(3)         VALUE
               '*'.
           05  WS-LCRE-DET-LIB-ED            PIC X(30).
           05  FILLER                        PIC X(3)         VALUE
               ':'.
           05  WS-LCRE-DET-TOT-ED            PIC Z(5)9.
           05  FILLER                        PIC X(3)         VALUE
               '  *'.
      *
      *DECLARATION DES COMPTEURS ET AUTRES VARIABLES DE TRAITEMENT
      *
      *COMPTEUR POUR LE COMPTE RENDU D EXECUTION INITIALISE A ZERO
      *-------------------------------------------------------------
       01  WS-CCLI                           PIC 9(3)         VALUE
                   0.
       01  WS-CMVT                           PIC 9(3)         VALUE
                   0.
       01  WS-CERR                           PIC 9(3)         VALUE
                   0.
       01  WS-CRET                           PIC 9(3)         VALUE
                   0.
       01  WS-CCB                            PIC 9(3)         VALUE
                   0.
       01  WS-CDEP                           PIC 9(3)         VALUE
                   0.
       01  WS-OCPT                           PIC 9(10).
      *
       01 WS-TOT-MTCR                        PIC 9(9)V99.
      *
       01 WS-TOT-MTDB                        PIC 9(9)V99.
      *
       01 WS-ANO-CPT                         PIC 9(4)         VALUE
                   0.
      *
       01 WS-ANO-TOT                         PIC 9(8)V99.
      *
       01 WS-MT-DEP                          PIC 9(8)V99.
      *
      *
      *  CUMUL DES TRANSACTIONS
      *------------------------
       01  WS-OCB                            PIC 9(9)V99.
      *
       01  WS-ORDAB                          PIC 9(9)V99.
      *
       01  WS-ODGUI                          PIC 9(9)V99.
      *
      *  DATE
       01 WS-DATE.
          05 WS-YY                           PIC 9999.
          05 WS-MM                           PIC 99.
          05 WS-DD                           PIC 99.
      *
      *  VARIABLE TAMPON
      *  ---------------
       01  WS-BUFFER                         PIC X(80).
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
       0000-TRAITEMENT-PRINCIPAL-DEB.
      *
      * DEBUT DE PROGRAMME
      *
      * OREILLETTE GAUCHE
      * OUVERTURE DE F-MVTS-E
           PERFORM  6000-OUVRIR-F-MVTS-E-DEB
              THRU  6000-OUVRIR-F-MVTS-E-FIN.
      * OUVERTURE DE F-ETATCLI-S
           PERFORM  6030-OUVRIR-CLIENT-DEB
              THRU  6030-OUVRIR-CLIENT-FIN.
      * OUVERTURE DE F-ETATANO-S
           PERFORM  6050-OUVRIR-F-ETATANO-S-DEB
              THRU  6050-OUVRIR-F-ETATANO-S-FIN.
      * LECTURE DE F-MVTS-E
           PERFORM  6010-LIRE-F-MVTS-E-DEB
              THRU  6010-LIRE-F-MVTS-E-FIN.
           IF WS-FS-MVTS-E  = '10'
              PERFORM 8000-EDIT-EOF-DEB
                 THRU 8000-EDIT-EOF-FIN
           END-IF.
      *  INITIALISER LES VALEURS
           PERFORM  7999-INIT-DATE-DEB
              THRU  7999-INIT-DATE-FIN.
      * APPEL DU COMPOSANT SUIVANT ( PREDICAT )
      *
           PERFORM  1000-TRAITEMENT-COMPTE-DEB
              THRU  1000-TRAITEMENT-COMPTE-FIN
             UNTIL  WS-FS-MVTS-E = '10'.
      *
      *FIN DU TRAITEMENT ( OREILLETTE DROITE )
      *
           IF WS-CERR NOT = 0
              PERFORM  7100-TOT-ANO-DEB
                 THRU  7100-TOT-ANO-FIN
              PERFORM  8060-EDIT-TOT-ETATANO-DEB
                 THRU  8060-EDIT-TOT-ETATANO-FIN
           END-IF.
      * EDIT TOTAL COMPTE RENDU
           PERFORM  7050-TOTAL-MOUVEMENT-DEB
              THRU  7050-TOTAL-MOUVEMENT-FIN.
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN
      * FERMETURE DES FICHIERS.
           PERFORM  6020-FERMER-F-MVTS-E-DEB
              THRU  6020-FERMER-F-MVTS-E-FIN.
      * FERMETURE F-ETATCLI
           PERFORM  6040-FERMER-CLIENT-DEB
              THRU  6040-FERMER-CLIENT-FIN.
      * FERMETURE F-ETATANO
           PERFORM  6060-FERMER-F-ETATANO-S-DEB
              THRU  6060-FERMER-F-ETATANO-S-FIN.
      * FIN DU PROGRAMME
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-TRAITEMENT-PRINCIPAL-FIN.
           STOP RUN.
       1000-TRAITEMENT-COMPTE-DEB.
      *
      *PREPARATION DE TRAITEMENT (OREILLETTE GAUCHE )
      *
           PERFORM 7000-INIT-COMPTE-DEB
              THRU 7000-INIT-COMPTE-FIN.
      *
      *APPEL DU COMPOSANT SUIVANT
      *
           PERFORM 2000-TRAITEMENT-MOUVEMENT-DEB
              THRU 2000-TRAITEMENT-MOUVEMENT-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-OCPT) OR
                   WS-FS-MVTS-E = '10'.
      *
      *TRAITEMENT DU COMPOSANT
           IF NOT (WS-TOT-MTCR = 0 AND WS-TOT-MTDB = 0)
              PERFORM 7060-TOT-CLI-DEB
                 THRU 7060-TOT-CLI-FIN
              PERFORM 8050-EDIT-TOT-ETATCLI-DEB
                 THRU 8050-EDIT-TOT-ETATCLI-FIN
           END-IF.
      *
       1000-TRAITEMENT-COMPTE-FIN.
           EXIT.
      *
       2000-TRAITEMENT-MOUVEMENT-DEB.
      *
           EVALUATE WS-MVTS-CODE
              WHEN 'D'
                   PERFORM 3000-TRT-DEPOT-DEB
                      THRU 3000-TRT-DEPOT-FIN
              WHEN 'C'
                   PERFORM 3010-TRT-CARTE-DEB
                      THRU 3010-TRT-CARTE-FIN
              WHEN 'R'
                   PERFORM 3020-TRT-RETRAIT-DEB
                      THRU 3020-TRT-RETRAIT-FIN
              WHEN OTHER
                   PERFORM 3030-TRT-ANOMALIE-DEB
                      THRU 3030-TRT-ANOMALIE-FIN
           END-EVALUATE.
      *
      *TRAITEMENT DU COMPOSANT EN SORTIE
           PERFORM 6010-LIRE-F-MVTS-E-DEB
              THRU 6010-LIRE-F-MVTS-E-FIN.
      *
      *
       2000-TRAITEMENT-MOUVEMENT-FIN.
           EXIT.
      *
      *
       3000-TRT-DEPOT-DEB.
      *
           IF (WS-TOT-MTDB = 0 AND WS-TOT-MTCR = 0)
              PERFORM 8010-EDIT-ENTETE-CLI-DEB
                 THRU 8010-EDIT-ENTETE-CLI-FIN
           END-IF.
           PERFORM 7010-DEPOT-DEB
              THRU 7010-DEPOT-FIN.
           PERFORM 8030-EDIT-DET-CLI-DEB
              THRU 8030-EDIT-DET-CLI-FIN.
       3000-TRT-DEPOT-FIN.
           EXIT.
      *
       3010-TRT-CARTE-DEB.
           IF (WS-TOT-MTDB = 0 AND WS-TOT-MTCR = 0)
              PERFORM 8010-EDIT-ENTETE-CLI-DEB
                 THRU 8010-EDIT-ENTETE-CLI-FIN
           END-IF.
           PERFORM 7020-CARTE-DEB
              THRU 7020-CARTE-FIN.
           PERFORM 8030-EDIT-DET-CLI-DEB
              THRU 8030-EDIT-DET-CLI-FIN.
       3010-TRT-CARTE-FIN.
           EXIT.
      *
       3020-TRT-RETRAIT-DEB.
           IF (WS-TOT-MTDB = 0 AND WS-TOT-MTCR = 0)
              PERFORM 8010-EDIT-ENTETE-CLI-DEB
                 THRU 8010-EDIT-ENTETE-CLI-FIN
           END-IF.
           PERFORM 7030-RETRAIT-DEB
              THRU 7030-RETRAIT-FIN.
           PERFORM 8030-EDIT-DET-CLI-DEB
              THRU 8030-EDIT-DET-CLI-FIN.
       3020-TRT-RETRAIT-FIN.
           EXIT.
      *
       3030-TRT-ANOMALIE-DEB.
           IF WS-CERR = 0
              PERFORM 8020-EDIT-ENTETE-ANO-DEB
                 THRU 8020-EDIT-ENTETE-ANO-FIN
           END-IF.
              PERFORM 7040-ANOMALIE-DEB
                 THRU 7040-ANOMALIE-FIN.
              PERFORM 8040-EDIT-DET-ETATANO-DEB
                 THRU 8040-EDIT-DET-ETATANO-FIN.
       3030-TRT-ANOMALIE-FIN.
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
       6000-OUVRIR-F-MVTS-E-DEB.
           OPEN INPUT F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
              THEN DISPLAY 'PROBLEME OUVERTURE FICHIER F-MVTS-E'
                   DISPLAY 'CODE RETOUR =' WS-FS-MVTS-E
           PERFORM 9999-ERREUR-PROGRAMME-DEB
              THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OUVRIR-F-MVTS-E-FIN.
           EXIT.
      *
       6010-LIRE-F-MVTS-E-DEB.
           READ F-MVTS-E INTO WS-MVTS-E
           IF NOT (WS-FS-MVTS-E = '00' OR '10')
              DISPLAY 'PROBLEME LECTURE F-MVTS-E'
              DISPLAY 'CODE RETOUR =' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6010-LIRE-F-MVTS-E-FIN.
           EXIT.
      *
       6020-FERMER-F-MVTS-E-DEB.
           CLOSE F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
              DISPLAY 'PROBLEME FERMETURE FICHIER F-MVTS-E'
              DISPLAY 'CODE RETOUR ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-FERMER-F-MVTS-E-FIN.
           EXIT.
      *
       6030-OUVRIR-CLIENT-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME OUVERTURE FICHIER F-ETATCLI-S'
              DISPLAY 'CODE RETOUR ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-OUVRIR-CLIENT-FIN.
           EXIT.
      *
       6040-FERMER-CLIENT-DEB.
           CLOSE F-ETATCLI-S.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME FERMETURE FICHIER F-ETATCLI-S'
              DISPLAY 'FILE STATUS ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-FERMER-CLIENT-FIN.
           EXIT.
      *
       6050-OUVRIR-F-ETATANO-S-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF  WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME OUVERTURE FICHIER F-ETATANO-S'
              DISPLAY 'FILE STATUS ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-OUVRIR-F-ETATANO-S-FIN.
           EXIT.
      *
       6060-FERMER-F-ETATANO-S-DEB.
           CLOSE F-ETATANO-S.
           IF  WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME FERMETURE FICHIER F-ETATANO-S'
              DISPLAY 'FILE STATUS ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-FERMER-F-ETATANO-S-FIN.
           EXIT.
      * ECRIRE ETATCLI AVEC SAUT DE PAGE
       6070-ECRIRE-CLI-SDP-DEB.
           WRITE FS-ENRG-ETATCLI-S
            FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF WS-FS-ETATCLI-S NOT = '00'
               DISPLAY 'ERREUR D ECRITURE ETATCLI'
               DISPLAY 'FILE STATUS' WS-FS-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-ECRIRE-CLI-SDP-FIN.
           EXIT.
      * ECRIRE ETATCLI
       6080-ECRIRE-CLI-DEB.
           WRITE FS-ENRG-ETATCLI-S
            FROM WS-BUFFER
           END-WRITE.
           IF WS-FS-ETATCLI-S NOT = '00'
               DISPLAY 'ERREUR D ECRITURE ETATCLI SANS SDP'
               DISPLAY 'FILE STATUS' WS-FS-ETATCLI-S
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
            END-IF.
       6080-ECRIRE-CLI-FIN.
           EXIT.
      *
       6090-ECRIRE-ANO-SDP-DEB.
           WRITE FS-ENRG-ETATANO-S
            FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'ERREUR D ECRITURE ETATANO AVEC SDP'
              DISPLAY 'FILE STATUS' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-ECRIRE-ANO-SDP-FIN.
           EXIT.
      *
       6100-ECRIRE-ANO-DEB.
           WRITE FS-ENRG-ETATANO-S
            FROM WS-BUFFER
           END-WRITE.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'ERREUR D ECRITURE ETATANO SANS SDP'
              DISPLAY 'FILE STATUS' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-ECRIRE-ANO-FIN.
           EXIT.
      *6000-ORDRE-FICHIER-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
       7000-INIT-COMPTE-DEB.
           MOVE WS-MVTS-CPTE           TO WS-OCPT.
           MOVE 0                      TO WS-TOT-MTDB WS-TOT-MTCR.
           ADD 1                       TO WS-CCLI.
           MOVE WS-MVTS-CPTE           TO WS-LETAT-AST-CPTE-ED.
       7000-INIT-COMPTE-FIN.
           EXIT.
      *
       7010-DEPOT-DEB.
           MOVE 'DEPOT GUICHET'        TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-DET-MTCR-ED.
           MOVE 0                      TO WS-LETAT-DET-MTDB-ED.
           ADD  WS-MVTS-MT             TO WS-TOT-MTCR.
           ADD 1                       TO WS-CDEP.
       7010-DEPOT-FIN.
           EXIT.
       7020-CARTE-DEB.
           MOVE 'CARTE BLEUE'          TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-DET-MTDB-ED.
           MOVE 0                      TO WS-LETAT-DET-MTCR-ED.
           ADD  WS-MVTS-MT             TO WS-TOT-MTDB.
           ADD 1                       TO WS-CCB.
       7020-CARTE-FIN.
           EXIT.
      *
       7030-RETRAIT-DEB.
           MOVE 'RETRAIT DAB'          TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-DET-MTDB-ED
           MOVE 0                      TO WS-LETAT-DET-MTCR-ED
           ADD  WS-MVTS-MT             TO WS-TOT-MTDB.
           ADD 1                       TO WS-CRET.
      *
       7030-RETRAIT-FIN.
           EXIT.
      *
       7040-ANOMALIE-DEB.
           MOVE WS-MVTS-CPTE           TO WS-LANO-DET-CPT-ED.
           MOVE WS-MVTS-CODE           TO WS-LANO-DET-MVT-ED.
           MOVE WS-MVTS-MT             TO WS-LANO-DET-MT-ED.
           ADD 1                       TO WS-CERR.
           ADD WS-MVTS-MT              TO WS-ANO-TOT.
       7040-ANOMALIE-FIN.
           EXIT.
      *
       7050-TOTAL-MOUVEMENT-DEB.
           ADD WS-CERR WS-CRET WS-CCB WS-CDEP
               GIVING WS-CMVT.
       7050-TOTAL-MOUVEMENT-FIN.
           EXIT.
      *
       7060-TOT-CLI-DEB.
           MOVE WS-TOT-MTDB            TO WS-LETAT-TOT-MTDB-ED.
           MOVE WS-TOT-MTCR            TO WS-LETAT-TOT-MTCR-ED.
       7060-TOT-CLI-FIN.
           EXIT.
      *
       7100-TOT-ANO-DEB.
           MOVE WS-ANO-TOT             TO WS-LANO-TOT-MT-ED.
       7100-TOT-ANO-FIN.
           EXIT.
       7999-INIT-DATE-DEB.
      *
           ACCEPT WS-DATE              FROM DATE YYYYMMDD.
           MOVE WS-DD                  TO WS-JOUR.
           MOVE WS-MM                  TO WS-MOIS.
           MOVE WS-YY                  TO WS-ANNEE.
       7999-INIT-DATE-FIN.
           EXIT.
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
       8000-EDIT-EOF-DEB.
           DISPLAY 'FICHIER F-MVTS-E VIDE'.
       8000-EDIT-EOF-FIN.
           EXIT.
      *
       8010-EDIT-ENTETE-CLI-DEB.
           MOVE WS-LETAT-ASTER         TO WS-BUFFER.
           PERFORM 6070-ECRIRE-CLI-SDP-DEB
              THRU 6070-ECRIRE-CLI-SDP-FIN.
           MOVE WS-LETAT-ENT           TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
           MOVE WS-LETAT-ASTER         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
           MOVE WS-LETAT-TITRE         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
           MOVE WS-LETAT-ASTER         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
       8010-EDIT-ENTETE-CLI-FIN.
           EXIT.
      *
      * EDITION DE L ENTETE DU FICHIER DES ANOMALIES.
      *
       8020-EDIT-ENTETE-ANO-DEB.
           MOVE WS-LANO-L1             TO WS-BUFFER.
           PERFORM 6090-ECRIRE-ANO-SDP-DEB
              THRU 6090-ECRIRE-ANO-SDP-FIN.
           MOVE WS-LANO-TITRE          TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN.
           MOVE WS-LANO-L3             TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN.
       8020-EDIT-ENTETE-ANO-FIN.
           EXIT.
      *
      * EDITION DU DETAIL DU MOUVEMENT
      *
       8030-EDIT-DET-CLI-DEB.
           MOVE WS-LETAT-DETAIL        TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
               THRU 6080-ECRIRE-CLI-FIN.
       8030-EDIT-DET-CLI-FIN.
           EXIT.
      *
      * EDITION DU DETAIL DU FICHIER ETATANO.
      *
       8040-EDIT-DET-ETATANO-DEB.
           MOVE WS-LANO-DETAIL         TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN.
       8040-EDIT-DET-ETATANO-FIN.
           EXIT.
      *
      * EDITION DU TOTAL ET FIN DE PAGE DU FICHIER ETATCLI
      *
       8050-EDIT-TOT-ETATCLI-DEB.
           MOVE WS-LETAT-ASTER         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
               THRU 6080-ECRIRE-CLI-FIN.
           MOVE WS-LETAT-TOTAL         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
           MOVE WS-LETAT-ASTER         TO WS-BUFFER.
           PERFORM 6080-ECRIRE-CLI-DEB
              THRU 6080-ECRIRE-CLI-FIN.
       8050-EDIT-TOT-ETATCLI-FIN.
           EXIT.
      *
      * EDITION DU TOTAL ET FIN DE PAGE DU FICHIER DES ANOMALIES
      *
       8060-EDIT-TOT-ETATANO-DEB.
           MOVE WS-LANO-L3             TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN
           MOVE WS-LANO-TOTAL          TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN.
           MOVE WS-LANO-L1             TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ANO-DEB
              THRU 6100-ECRIRE-ANO-FIN.
       8060-EDIT-TOT-ETATANO-FIN.
           EXIT.
      *
       8070-EDIT-PROBLEME-DEB.
            DISPLAY 'PROBLEME D OUVERTURE FICHIER D ECRITURE'.
       8070-EDIT-PROBLEME-FIN.
           EXIT.
      * EDITION DU COMPTE RENDU
      *
       8999-STATISTIQUES-DEB.
           DISPLAY '************************************************'.
           DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO249        *'.
           DISPLAY '*     =================================        *'.
           DISPLAY '************************************************'.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           MOVE 'NOMBRE DE CLIENTS'    TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CCLI                TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE MOUVEMENTS' TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CMVT                TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE MOUVEMENTS ERRONES'
                                       TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CERR                TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE RETRAITS'   TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CRET                TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE CARTES BLEUES'
                                       TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CCB                 TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE DEPOTS'     TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CDEP                TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           DISPLAY WS-LCRE-ASTER.
      *
       8999-STATISTIQUES-FIN.
           EXIT.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
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
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO249         *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO249        *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
