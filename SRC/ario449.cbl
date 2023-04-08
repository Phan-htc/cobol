      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO449                                   *
      *  NOM DU REDACTEUR : PHAN                                      *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 17/02/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * METTRE A JOUR DES COMPTES CLIENTS A PARTIR DES MOUVEMENTS     *
      * BANCAIRES.
      *
      * POSSIBILITE DE CLOTURER UN COMPTE.                            *
      *                                                               *
      * EDITER DEUX FICHIERS EN FIN DE PROGRAMME :                    *
      *                                                               *
      * - ETATCLI : AFFICHE A CHAQUE PAGE UN CLIENT VIA SON NUMERO DE *
      *             COMPTE, LES DETAILS DE CHAQUE OPERATION VALIDE    *
      *             ( DATE, MONTANT DU DEBIT OU DU CREDIT) ET SON     *
      *             SOLDE AVANT ET APRES LE TRAITEMENT DU PROGRAMME   *
      *                                                               *
      * - ETATANO : AFFICHE TOUT LES MOUVEMENTS AYANT UN CODE AUTRE   *
      *             QUE C / D / R / K AVEC LE NUMERO DU COMPTE,       *
      *             LE CODE MOUVEMENT ET LE MONTANT.                  *
      *             EN FIN DE PROGRAMME, IL AFFICHERA LE MONTANT      *
      *             TOTAL DES ANOMALIES.                              *
      *                                                               *
      * EDITERA EN FIN DE PROGRAMME UN COMPTE RENDU D'EXECUTION POUR  *
      * L'EQUIPE D'EXPLOITATION.                                      *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      *   /  /        !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO449.
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
      *                      CPTE : FICHIER DES COMPTES CLIENT
      *                      -------------------------------------------
           SELECT  F-CPTE-ES           ASSIGN TO IO001
                   ORGANIZATION        IS INDEXED
                   ACCESS MODE         IS RANDOM
                   RECORD KEY          IS FS-CPT-CPTE
                   FILE STATUS         IS WS-FS-CPTE-ES.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER ETAT CLIENT
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S         ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETATCLI-S.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATANO : FICHIER DES ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S         ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETATANO-S.
      *                      -------------------------------------------
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
       01  FS-ENRG-MVTS-E               PIC X(50).
      *
       FD  F-CPTE-ES
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-CPTE.
           05  FS-CPT-CPTE              PIC X(10).
           05  FILLER                   PIC X(40).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI              PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO              PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      * FILES STATUS
      *-------------
       77  WS-FS-MVTS-E                 PIC X(2).
           88 M-SUCCES                      VALUE '00'.
           88 M-EOF                         VALUE '10'.
      *
       77  WS-FS-CPTE-ES                PIC X(2).
           88 CE-SUCCES                     VALUE '00'.
           88 CE-EMPTY                      VALUE '23'.
      *
       77  WS-FS-ETATCLI-S              PIC X(2).
           88 EC-SUCCES                     VALUE '00'.
      *
       77  WS-FS-ETATANO-S              PIC X(2).
           88 EA-SUCCES                     VALUE '00'.
      *--------------------------
      * VARIABLES FICHIER EXTERNE
      *--------------------------
       COPY TP4LEDIT.
      *
       COPY TP4MVTS.
      *
       COPY TP4CPT.
      *
      *
      *
       77  WS-BUFFER                    PIC X(80).
       77  WS-CLOTURE                   PIC 9.
           88  CLOTURE-TRUE                 VALUE 1.
           88  CLOTURE-FALSE                VALUE 0.
       77  WS-NB-LIGNE                  PIC S9(4)         COMP.
           88  LIGNE-MIN                    VALUE 0.
           88  LIGNE-MAX                    VALUE 5.
       77  WS-NB-PAGE                   PIC 9             COMP.
       77  WS-R-CPT                     PIC 9(10)         COMP.
      *-----
      * DATE
      *-----
       01  WS-DATE.
           05 WS-ANNEE.
              10 WS-SS                  PIC 99.
              10 WS-YY                  PIC 99.
           05 WS-MM                     PIC 99.
           05 WS-DD                     PIC 99.
      *
      * VARIABLE POUR CALCULER UN MOUVEMENT
      *
       77  WS-TOTDB                     PIC S9(11)V99 COMP-3.
           88  MTDB-NUL                                    VALUE 0.
       77  WS-TOTCR                     PIC S9(11)V99 COMP-3.
           88  MTCR-NUL                                    VALUE 0.
       77  WS-TOT-ANO                   PIC S9(11)V99 COMP-3
                                                           VALUE 0.
       77  WS-OCPT                      PIC 9(10).
      *
       77  WS-SOLDE-CALC                PIC S9(11)V99 COMP-3.
       77  WS-SOLDE-TEMP                PIC S9(11)V99 COMP-3.
      *
      * CUMUL POUR LE COMPTE RENDU D EXECUTION
      *
       77  WS-CCLI                      PIC S9(4)     COMP VALUE 0.
       77  WS-CCLIN                     PIC S9(4)     COMP VALUE 0.
       77  WS-CCLICLO                   PIC S9(4)     COMP VALUE 0.
       77  WS-CCLISTD                   PIC S9(4)     COMP VALUE 0.
      *
       77  WS-CMVT                      PIC S9(4)     COMP VALUE 0.
       77  WS-CERR                      PIC S9(4)     COMP VALUE 0.
           88  ERR-ZERO                                    VALUE 0.
       77  WS-CDEP                      PIC S9(4)     COMP VALUE 0.
       77  WS-CCB                       PIC S9(4)     COMP VALUE 0.
       77  WS-CRET                      PIC S9(4)     COMP VALUE 0.
       77  WS-CLOT                      PIC S9(4)     COMP VALUE 0.
      *
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
       0000-PROGRAMME-DEB.
      *
      * DEBUT DU PROGRAMME (OREILLETTE GAUCHE LVL 0000)
      *
      * OUVERTURE FICHIERS
           PERFORM  6000-OUVRIR-FMVTS-DEB
              THRU  6000-OUVRIR-FMVTS-FIN.
      *
           PERFORM  6010-OUVRIR-FCPTE-DEB
              THRU  6010-OUVRIR-FCPTE-FIN.
      *
           PERFORM  6020-OUVRIR-ETATCLI-DEB
              THRU  6020-OUVRIR-ETATCLI-FIN.
      *
           PERFORM  6030-OUVRIR-ETATANO-DEB
              THRU  6030-OUVRIR-ETATANO-FIN.
      *
           PERFORM  6040-LIRE-FMVTS-DEB
              THRU  6040-LIRE-FMVTS-FIN.
           IF M-EOF
              DISPLAY 'FICHIER MVTS VIDE'
           END-IF.
      *
           PERFORM  7999-INIT-DATE-DEB
              THRU  7999-INIT-DATE-FIN.
      *
           PERFORM  8000-EDIT-PDG-ETATCLI-DEB
              THRU  8000-EDIT-PDG-ETATCLI-FIN.
      *
           PERFORM  8010-EDIT-PDG-ETATANO-DEB
              THRU  8010-EDIT-PDG-ETATANO-FIN.
      *
      * ITERATIVE LVL 0000
      *
      * APPEL DU COMPOSANT SUIVANT TANT QUE
           PERFORM  1000-TRT-COMPTE-DEB
              THRU  1000-TRT-COMPTE-FIN
             UNTIL  M-EOF.
      *
      * TRAITEMENT DU COMPOSANT (OREILLETTE DROITE LVL 0000)
      *
      * EFFECTUER LES CALCULS CONCERNANT LE :
           PERFORM  7060-TOTAL-COMPTE-RENDU-DEB
              THRU  7060-TOTAL-COMPTE-RENDU-FIN.
           IF NOT ERR-ZERO
           THEN
              PERFORM  8070-EDIT-TOT-ANO-DEB
                 THRU  8070-EDIT-TOT-ANO-FIN
           ELSE
              PERFORM  8080-EDIT-ANO-OK-DEB
                 THRU  8080-EDIT-ANO-OK-FIN
           END-IF.
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN.
      *
           PERFORM  6060-FERMER-MVTS-DEB
              THRU  6060-FERMER-MVTS-FIN.
      *
           PERFORM  6070-FERMER-CPTE-DEB
              THRU  6070-FERMER-CPTE-FIN.
      *
           PERFORM  6080-FERMER-ETATCLI-DEB
              THRU  6080-FERMER-ETATCLI-FIN.
      *
           PERFORM  6090-FERMER-ETATANO-DEB
              THRU  6090-FERMER-ETATANO-FIN.
      *
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
           STOP RUN.
      *
      * TRAITEMENT COMPTE
      *
       1000-TRT-COMPTE-DEB.
      *
      * (OREILLETTE GAUCHE LVL 1000)
      *
           PERFORM 7170-INIT-CPT-PRI-KEY-DEB
              THRU 7170-INIT-CPT-PRI-KEY-FIN.
      *
           PERFORM 6050-LIRE-CPTE-DEB
              THRU 6050-LIRE-CPTE-FIN.
      * (ITERATIVE)
           IF CE-SUCCES
      *
              PERFORM  2000-MAJ-COMPTE-DEB
                 THRU  2000-MAJ-COMPTE-FIN
      *
           ELSE
              PERFORM  2010-CREA-COMPTE-DEB
                 THRU  2010-CREA-COMPTE-FIN
      *     
           END-IF.
       1000-TRT-COMPTE-FIN.
           EXIT.
      *     
      * FIN TRAITEMENT COMPTE
      * ---------------------
      *     
      * COMPTE EXISTANT
      *     
       2000-MAJ-COMPTE-DEB.
      *     
      * (OREILLETTE GAUCHE LVL 2000)
           PERFORM 7000-INIT-COMPTE-MAJ-DEB
              THRU 7000-INIT-COMPTE-MAJ-FIN.
      *     
      * APPEL DE L ASSORTIMENT
           PERFORM 3000-TRT-MOUVEMENT-MAJ-DEB
              THRU 3000-TRT-MOUVEMENT-MAJ-FIN
             UNTIL M-EOF OR
                   WS-MVTS-CPTE NOT = WS-CPTS-CPTE.
      *     
      * (OREILLETTE DROITE LVL 2000)
           IF CLOTURE-TRUE
              PERFORM 7040-CLOTURE-DEB
                 THRU 7040-CLOTURE-FIN
              PERFORM 6160-SUPPRESSION-CPTE-DEB
                 THRU 6160-SUPPRESSION-CPTE-FIN
           ELSE
      * CLIENT STANDARD
              PERFORM 7130-CLIENT-STANDARD-DEB
                 THRU 7130-CLIENT-STANDARD-FIN
              PERFORM 6150-REECRIRE-CPTE-DEB
                 THRU 6150-REECRIRE-CPTE-FIN
           END-IF.
           IF WS-TOTCR NOT = 0 OR WS-TOTDB NOT = 0
              PERFORM 7120-TOTAL-NOUVEAU-SOLDE-DEB
                 THRU 7120-TOTAL-NOUVEAU-SOLDE-FIN
              PERFORM 8040-EDIT-TOT-ETATCLI-DEB
                 THRU 8040-EDIT-TOT-ETATCLI-FIN
           END-IF.
      *     
       2000-MAJ-COMPTE-FIN.
           EXIT.
      *     
      *COMPTE INEXISTANT
      *     
       2010-CREA-COMPTE-DEB.
      *     
      * (OREILLETTE GAUCHE LVL 2010)
      *     
           PERFORM 7070-INIT-CREA-CPTE-DEB
              THRU 7070-INIT-CREA-CPTE-FIN.
      *     
      * ITERATIVE DE CORRESPONDANCE DES NUMCPT ENTRE MVTS ET CPTE
           PERFORM 3010-TRT-MVT-CREATION-DEB
              THRU 3010-TRT-MVT-CREATION-FIN
             UNTIL M-EOF OR
                   WS-CPTS-CPTE NOT = WS-MVTS-CPTE 
      *     
      * (OREILLETTE DROITE LVL 2010)
           IF NOT (MTDB-NUL AND MTCR-NUL)
               PERFORM 7100-TOTAL-NEW-CLIENT-DEB
                  THRU 7100-TOTAL-NEW-CLIENT-FIN
               PERFORM 7120-TOTAL-NOUVEAU-SOLDE-DEB
                  THRU 7120-TOTAL-NOUVEAU-SOLDE-FIN
      *     
              IF CLOTURE-FALSE
                 PERFORM 6100-ECRIRE-CPTS-DEB
                    THRU 6100-ECRIRE-CPTS-FIN
              ELSE
                 PERFORM 7040-CLOTURE-DEB
                    THRU 7040-CLOTURE-FIN
              END-IF
               PERFORM 8040-EDIT-TOT-ETATCLI-DEB
                  THRU 8040-EDIT-TOT-ETATCLI-FIN
           END-IF.
      *     
       2010-CREA-COMPTE-FIN.
           EXIT.
      *     
      *     
       3000-TRT-MOUVEMENT-MAJ-DEB.
      *                                              
      * (OREILLETTE GAUCHE LVL 3000)                 
      *                                              
           IF CB OR RETRAIT OR DEPOT                 
              IF LIGNE-MAX                           
      *                                              
                 PERFORM 7140-SOUS-TOTAL-CLI-DEB     
                    THRU 7140-SOUS-TOTAL-CLI-FIN     
      *                                              
                 PERFORM 8040-EDIT-TOT-ETATCLI-DEB   
                    THRU 8040-EDIT-TOT-ETATCLI-FIN   
              END-IF                                 
              IF LIGNE-MIN                           
      *                                              
                 PERFORM 7090-PREPA-ENT-CLI-DEB      
                    THRU 7090-PREPA-ENT-CLI-FIN      
                 PERFORM 8020-EDIT-ET-DET-ETATCLI-DEB
                    THRU 8020-EDIT-ET-DET-ETATCLI-FIN
              END-IF                                 
           END-IF.                                   
      *                                              
      * ITERATIVE CORRESPONDANCE NUM COMPTE MOUVEMENT ET COMPTE.
      *                                              
           EVALUATE TRUE                             
                    WHEN DEPOT                       
                         PERFORM 4000-TRT-DEPOT-DEB  
                            THRU 4000-TRT-DEPOT-FIN  
      *                                              
                    WHEN CB
                         PERFORM 4010-TRT-CARTE-DEB
                            THRU 4010-TRT-CARTE-FIN
      *     
                    WHEN RETRAIT
                         PERFORM 4020-TRT-RETRAIT-DEB
                            THRU 4020-TRT-RETRAIT-FIN
      *     
                    WHEN CLOTURE
                         PERFORM 4030-TRT-CLOTURE-DEB
                            THRU 4030-TRT-CLOTURE-FIN
      *     
                    WHEN OTHER
                         PERFORM 4040-TRT-ANOMALIE-DEB
                            THRU 4040-TRT-ANOMALIE-FIN
           END-EVALUATE.
      *     
      * (OREILLETTE DROITE LVL 3000)
      *     
           IF ( RETRAIT OR CB OR DEPOT)
              PERFORM 8030-EDIT-DETAIL-ETATCLI-DEB
                 THRU 8030-EDIT-DETAIL-ETATCLI-FIN
           END-IF.
           PERFORM 6040-LIRE-FMVTS-DEB
              THRU 6040-LIRE-FMVTS-FIN.
      *     
       3000-TRT-MOUVEMENT-MAJ-FIN.
           EXIT.
      *     
       3010-TRT-MVT-CREATION-DEB.
      *     
      * (OREILLETTE GAUCHE LVL 3010)
           IF CB OR RETRAIT OR DEPOT
              IF LIGNE-MAX
      *     
                 PERFORM 7140-SOUS-TOTAL-CLI-DEB
                    THRU 7140-SOUS-TOTAL-CLI-FIN
      *     
                 PERFORM 8040-EDIT-TOT-ETATCLI-DEB
                    THRU 8040-EDIT-TOT-ETATCLI-FIN
              END-IF
              IF LIGNE-MIN
      *     
                 PERFORM 7090-PREPA-ENT-CLI-DEB
                    THRU 7090-PREPA-ENT-CLI-FIN
                 PERFORM 8020-EDIT-ET-DET-ETATCLI-DEB
                    THRU 8020-EDIT-ET-DET-ETATCLI-FIN
              END-IF
           END-IF.
      *     
           IF WS-TOTDB = 0 AND WS-TOTCR = 0
              PERFORM 7160-INIT-DCREA-DEB
                 THRU 7160-INIT-DCREA-FIN
           END-IF.
      *     
      *    QUEL EST LE CODE MOUVEMENT ?
      *     
           EVALUATE TRUE
                    WHEN DEPOT
                         PERFORM 4000-TRT-DEPOT-DEB
                            THRU 4000-TRT-DEPOT-FIN
      *     
                    WHEN CB
                         PERFORM 4010-TRT-CARTE-DEB
                            THRU 4010-TRT-CARTE-FIN
      *     
                    WHEN RETRAIT
                         PERFORM 4020-TRT-RETRAIT-DEB
                            THRU 4020-TRT-RETRAIT-FIN
      *     
                    WHEN CLOTURE
                         PERFORM 4030-TRT-CLOTURE-DEB
                            THRU 4030-TRT-CLOTURE-FIN
     *      
                    WHEN OTHER
                         PERFORM 4040-TRT-ANOMALIE-DEB
                            THRU 4040-TRT-ANOMALIE-FIN
           END-EVALUATE.
      *     
      * (OREILLETTE DROITE LVL 3010)
      *     
           IF ( RETRAIT OR CB OR DEPOT)
              PERFORM 8030-EDIT-DETAIL-ETATCLI-DEB
                 THRU 8030-EDIT-DETAIL-ETATCLI-FIN
           END-IF.
           PERFORM 6040-LIRE-FMVTS-DEB
              THRU 6040-LIRE-FMVTS-FIN.
      *     
       3010-TRT-MVT-CREATION-FIN.
           EXIT.
      *     
      *     
       4000-TRT-DEPOT-DEB.
      *     
           PERFORM 7010-DEPOT-DEB
              THRU 7010-DEPOT-FIN.
      *     
       4000-TRT-DEPOT-FIN.
           EXIT.
      *     
       4010-TRT-CARTE-DEB.
      *     
           PERFORM 7030-CARTE-DEB
              THRU 7030-CARTE-FIN.
      *     
       4010-TRT-CARTE-FIN.
           EXIT.
      *     
       4020-TRT-RETRAIT-DEB.
      *     
           PERFORM 7020-RETRAIT-DEB
              THRU 7020-RETRAIT-FIN.
      *     
       4020-TRT-RETRAIT-FIN.
           EXIT.
      *     
       4030-TRT-CLOTURE-DEB.
      *     
           PERFORM 7110-MVT-CLOTURE-DEB
              THRU 7110-MVT-CLOTURE-FIN.
      *     
       4030-TRT-CLOTURE-FIN.
           EXIT.
      *     
       4040-TRT-ANOMALIE-DEB.
           PERFORM 7050-ANOMALIE-DEB
              THRU 7050-ANOMALIE-FIN.
       4040-TRT-ANOMALIE-FIN.
           EXIT.
      *     
           IF WS-CERR = 0
      *     
              PERFORM 8050-EDIT-ET-DET-ANO-DEB
                 THRU 8050-EDIT-ET-DET-ANO-FIN
           END-IF.
           PERFORM 8060-EDIT-DET-ANO-DEB
              THRU 8060-EDIT-DET-ANO-FIN.
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
      *     
      *     
       6000-OUVRIR-FMVTS-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT M-SUCCES
              DISPLAY  'ERREUR OUVERTURE F-MVTS'
              DISPLAY  'CODE ERREUR : ' WS-FS-MVTS-E
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6000-OUVRIR-FMVTS-FIN.
           EXIT.
      *     
       6010-OUVRIR-FCPTE-DEB.
           OPEN I-O F-CPTE-ES.
           IF NOT CE-SUCCES
              DISPLAY  'ERREUR OUVERTURE F-CPTE'
              DISPLAY  'CODE ERREUR : ' WS-FS-CPTE-ES
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OUVRIR-FCPTE-FIN.
           EXIT.
      *     
       6020-OUVRIR-ETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT EC-SUCCES
              DISPLAY  'ERREUR OUVERTURE F-ETATCLI'
              DISPLAY  'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6020-OUVRIR-ETATCLI-FIN.
           EXIT.
      *     
       6030-OUVRIR-ETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S
           IF NOT EA-SUCCES
              DISPLAY  'ERREUR OUVERTURE F-ETATANO'
              DISPLAY  'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6030-OUVRIR-ETATANO-FIN.
           EXIT.
      *     
      *     
       6040-LIRE-FMVTS-DEB.
           READ F-MVTS-E               INTO WS-ENRG-F-MVTS.
           IF NOT M-SUCCES AND NOT M-EOF
           THEN
              DISPLAY  'ERREUR LECTURE F-MVTS'
              DISPLAY  'CODE ERREUR : ' WS-FS-MVTS-E
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
           IF M-EOF
           THEN
              SET WS-MVTS-CPTE-MAX             TO TRUE
           END-IF.
       6040-LIRE-FMVTS-FIN.
           EXIT.
      *     
       6050-LIRE-CPTE-DEB.
      *     
           READ F-CPTE-ES              INTO WS-ENRG-F-CPTS
           END-READ.
           IF NOT CE-SUCCES AND NOT CE-EMPTY
           THEN
              DISPLAY  'ERREUR LECTURE F-CPTE'
              DISPLAY  'CODE ERREUR' WS-FS-CPTE-ES
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6050-LIRE-CPTE-FIN.
           EXIT.
      *     
       6060-FERMER-MVTS-DEB.
           CLOSE F-MVTS-E.
           IF NOT M-SUCCES
              DISPLAY  'ERREUR FERMETURE F-MVTS'
              DISPLAY  'CODE ERREUR : ' WS-FS-MVTS-E
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6060-FERMER-MVTS-FIN.
           EXIT.
      *     
       6070-FERMER-CPTE-DEB.
           CLOSE F-CPTE-ES.
           IF NOT CE-SUCCES
              DISPLAY  'ERREUR FERMETURE F-CPTE-ES'
              DISPLAY  'CODE ERREUR : ' WS-FS-CPTE-ES
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6070-FERMER-CPTE-FIN.
           EXIT.
      *     
       6080-FERMER-ETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT EC-SUCCES
              DISPLAY  'ERREUR FERMETURE F-ETATCLI'
              DISPLAY  'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6080-FERMER-ETATCLI-FIN.
           EXIT.
      *     
       6090-FERMER-ETATANO-DEB.
           CLOSE F-ETATANO-S
           IF NOT EA-SUCCES
              DISPLAY  'ERREUR FERMETURE F-ETATANO'
              DISPLAY  'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM  9999-ERREUR-PROGRAMME-DEB
                 THRU  9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6090-FERMER-ETATANO-FIN.
           EXIT.
      *     
       6100-ECRIRE-CPTS-DEB.
      *     
           WRITE FS-ENRG-CPTE
            FROM WS-ENRG-F-CPTS
           END-WRITE.
           IF NOT CE-SUCCES
              DISPLAY 'ERREUR D ECRITURE ENRG-CPT'
              DISPLAY 'CODE ERREUR : ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6100-ECRIRE-CPTS-FIN.
           EXIT.
      *     
       6110-ECRIRE-ETATCLI-SDP-DEB.
           WRITE FS-ENRG-ETATCLI
            FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT EC-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATCLI-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6110-ECRIRE-ETATCLI-SDP-FIN.
           EXIT.
      *     
       6120-ECRIRE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
            FROM WS-BUFFER
           END-WRITE.
           IF NOT EC-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATCLI-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6120-ECRIRE-ETATCLI-FIN.
           EXIT.
      *     
       6130-ECRIRE-ETATANO-SDP-DEB.
           WRITE FS-ENRG-ETATANO
            FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATANO'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-ECRIRE-ETATANO-SDP-FIN.
           EXIT.
      *     
       6140-ECRIRE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
            FROM WS-BUFFER
           END-WRITE.
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATANO'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *     
       6140-ECRIRE-ETATANO-FIN.
           EXIT.
      *     
       6150-REECRIRE-CPTE-DEB.
           REWRITE FS-ENRG-CPTE
              FROM WS-ENRG-F-CPTS
           END-REWRITE.
           IF NOT CE-SUCCES
              DISPLAY 'ERREUR DE REECRITURE FICHIER CLIENT'
              DISPLAY 'CODE ERREUR : ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6150-REECRIRE-CPTE-FIN.
           EXIT.
      *     
       6160-SUPPRESSION-CPTE-DEB.
           DELETE F-CPTE-ES.
           IF NOT CE-SUCCES
              DISPLAY 'ERREUR DE SUPPRESSION FICHIER F-CPTE-ES'
              DISPLAY 'CODE ERREUR : ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6160-SUPPRESSION-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *     
       7000-INIT-COMPTE-MAJ-DEB.
           MOVE 0                              TO WS-TOTDB.
           MOVE 0                              TO WS-TOTCR.
           MOVE WS-DATE                        TO WS-CPTS-DMAJ.
           MOVE WS-CPTS-SOLDE                  TO WS-SOLDE-CALC.
           MOVE WS-MVTS-CPTE                   TO WS-LETAT-NUMCPT-ED.
           MOVE SPACES                         TO WS-LETAT-OPEN-ED.
       7000-INIT-COMPTE-MAJ-FIN.
           EXIT.
      *     
       7010-DEPOT-DEB.
           MOVE WS-MVTS-MT                     TO
                                               WS-LETAT-OP-CREDIT-ED.
           MOVE 0                              TO
                                               WS-LETAT-OP-DEBIT-ED.
           MOVE 'DEPOT GUICHET'                TO WS-LETAT-OP-LIB-ED.
           ADD  WS-MVTS-MT                     TO WS-TOTCR.
           ADD  1                              TO WS-CDEP.
           ADD  1                              TO WS-NB-LIGNE.
           MOVE WS-MVTS-SS                     TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                     TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                     TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                     TO WS-LETAT-OP-JJ-ED.
      *                                          
       7010-DEPOT-FIN.                           
           EXIT.                                 
      *                                          
      *                                          
       7020-RETRAIT-DEB.                         
      *                                          
           MOVE WS-MVTS-MT                     TO
                                               WS-LETAT-OP-DEBIT-ED.
           MOVE 0                              TO
                                               WS-LETAT-OP-CREDIT-ED.
           MOVE 'RETRAIT DAB'                  TO WS-LETAT-OP-LIB-ED.
           ADD  WS-MVTS-MT                     TO WS-TOTDB.
           ADD  1                              TO WS-CRET.
           MOVE WS-MVTS-SS                     TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                     TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                     TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                     TO WS-LETAT-OP-JJ-ED.
           ADD  1                              TO WS-NB-LIGNE.
      *                                          
       7020-RETRAIT-FIN.                         
           EXIT.                                 
      *                                          
      *                                          
       7030-CARTE-DEB.                           
      *                                          
           MOVE WS-MVTS-MT                     TO
                                               WS-LETAT-OP-DEBIT-ED.
           MOVE 0                              TO
                                               WS-LETAT-OP-CREDIT-ED.
           MOVE 'CARTE BLEUE'                  TO WS-LETAT-OP-LIB-ED.
           ADD  WS-MVTS-MT                     TO WS-TOTDB.
           ADD  1                              TO WS-CCB.
           ADD  1                              TO WS-NB-LIGNE.
           MOVE WS-MVTS-SS                     TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                     TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                     TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                     TO WS-LETAT-OP-JJ-ED.
      *
       7030-CARTE-FIN.
           EXIT.
      *
      *
       7040-CLOTURE-DEB.
      *
           ADD 1                               TO WS-CCLICLO.
           COMPUTE WS-SOLDE-CALC =
                   WS-SOLDE-CALC - WS-TOTDB + WS-TOTCR.
           MOVE WS-SOLDE-CALC                  TO WS-CPTS-SOLDE.
           MOVE 'CLOTURE DE COMPTE'            TO WS-LETAT-CLOSE-ED.
      *
       7040-CLOTURE-FIN.
           EXIT.
      *
      *
       7050-ANOMALIE-DEB.
      *
           ADD  1                              TO WS-CERR.
           ADD  WS-MVTS-MT                     TO WS-TOT-ANO.
           MOVE WS-MVTS-CPTE                   TO WS-LANO-NUMCPT-ED.
           MOVE WS-MVTS-CODE                   TO WS-LANO-CODEMVT-ED.
           MOVE WS-MVTS-MT                     TO WS-LANO-MONTANT-ED.
      *
       7050-ANOMALIE-FIN.
           EXIT.
      *
       7050-CREA-CPT-DEB.
      *
           MOVE 'ANCIEN SOLDE'                 TO WS-LETAT-LIB-ED.
           MOVE WS-MVTS-CPTE                   TO WS-CPTS-CPTE.
           MOVE WS-MVTS-DATE                   TO WS-CPTS-DCREA.
           MOVE WS-OCPT                        TO WS-LETAT-NUMCPT-ED.
           MOVE 0                              TO WS-LETAT-SOLD-ED.
      *
       7050-CREA-CPT-FIN.
           EXIT.
      *
       7060-TOTAL-COMPTE-RENDU-DEB.
      *
           COMPUTE WS-CCLI = WS-CCLIN + WS-CCLISTD + WS-CCLICLO.
           ADD  WS-CERR WS-CDEP WS-CRET WS-CCB WS-CLOT
                GIVING WS-CMVT.
           MOVE WS-CCLI                        TO WS-LANO-CLI-TOT-ED.
           MOVE WS-CMVT                        TO WS-LANO-MVT-TOT-ED.
      *
       7060-TOTAL-COMPTE-RENDU-FIN.
           EXIT.
      *
       7070-INIT-CREA-CPTE-DEB.
           MOVE 0                              TO WS-TOTDB
                                                  WS-TOTCR
                                                  WS-SOLDE-CALC.
           MOVE WS-DATE                        TO WS-CPTS-DMAJ.
           MOVE WS-MVTS-CPTE                   TO WS-CPTS-CPTE.
           MOVE 'CREATION DE COMPTE'           TO WS-LETAT-OPEN-ED.
       7070-INIT-CREA-CPTE-FIN.
           EXIT.
      *
       7080-TOTAL-CLIENT-DEB.
           MOVE WS-TOTCR                       TO WS-LETAT-TOTCR-ED.
           MOVE WS-TOTDB                       TO WS-LETAT-TOTDB-ED.
           COMPUTE WS-CPTS-SOLDE = WS-CPTS-SOLDE +
                                   WS-TOTCR -
                                   WS-TOTDB.
           MOVE WS-SOLDE-CALC                  TO WS-LETAT-SOLD-ED.
           MOVE 'NOUVEAU SOLDE'                TO WS-LETAT-LIB-ED.
           MOVE 'TOTAL DES OPERATIONS'         TO WS-LETAT-LIB-TOT-ED.
       7080-TOTAL-CLIENT-FIN.
           EXIT.
      *
      *
       7090-PREPA-ENT-CLI-DEB.
      *
           MOVE WS-MVTS-CPTE                   TO WS-CPTS-CPTE.
           MOVE WS-MVTS-CPTE                   TO WS-LETAT-NUMCPT-ED.
           MOVE 'ANCIEN SOLDE'                 TO WS-LETAT-LIB-ED.
           MOVE WS-NB-PAGE                     TO WS-LETAT-PAGE-ED.
           MOVE WS-SOLDE-CALC                  TO WS-LETAT-SOLD-ED.
       7090-PREPA-ENT-CLI-FIN.
           EXIT.
      *
       7100-TOTAL-NEW-CLIENT-DEB.
      *
           COMPUTE WS-SOLDE-CALC =
                   WS-SOLDE-CALC - WS-TOTDB + WS-TOTCR.
           MOVE WS-SOLDE-CALC                  TO WS-CPTS-SOLDE.
           ADD  1                              TO WS-CCLIN.
       7100-TOTAL-NEW-CLIENT-FIN.
           EXIT.
      *
       7110-MVT-CLOTURE-DEB.
      *
           SET CLOTURE-TRUE                    TO TRUE.
           ADD 1                               TO WS-CLOT.
       7110-MVT-CLOTURE-FIN.
           EXIT.
      *
       7120-TOTAL-NOUVEAU-SOLDE-DEB.
      *
           MOVE WS-TOTCR                       TO WS-LETAT-TOTCR-ED.
           MOVE WS-TOTDB                       TO WS-LETAT-TOTDB-ED.
           MOVE WS-CPTS-SOLDE                  TO WS-LETAT-SOLD-ED.
           MOVE 'NOUVEAU SOLDE'                TO WS-LETAT-LIB-ED.
           MOVE 'TOTAL DES OPERATIONS'         TO WS-LETAT-LIB-TOT-ED.
      *
       7120-TOTAL-NOUVEAU-SOLDE-FIN.
           EXIT.
      *
       7130-CLIENT-STANDARD-DEB.
           COMPUTE WS-SOLDE-CALC =
                   WS-SOLDE-CALC + WS-TOTCR - WS-TOTDB.
           MOVE WS-SOLDE-CALC                  TO WS-CPTS-SOLDE.
           ADD  1                              TO WS-CCLISTD.
       7130-CLIENT-STANDARD-FIN.
           EXIT.
      *
       7140-SOUS-TOTAL-CLI-DEB.
      *
           MOVE WS-TOTDB                       TO WS-LETAT-TOTDB-ED.
           MOVE WS-TOTCR                       TO WS-LETAT-TOTCR-ED.
           MOVE 'SOUS TOTAL DES OPERATIONS'    TO WS-LETAT-LIB-TOT-ED.
           MOVE 'SOLDE INTERMEDIAIRE'          TO WS-LETAT-LIB-ED.
           COMPUTE WS-SOLDE-TEMP =
                   WS-CPTS-SOLDE + WS-TOTCR - WS-TOTDB.
           MOVE WS-SOLDE-TEMP                  TO WS-LETAT-SOLD-ED.
           ADD  1                              TO WS-NB-PAGE.
           MOVE 0                              TO WS-NB-LIGNE.
      *
       7140-SOUS-TOTAL-CLI-FIN.
           EXIT.
      *
       7160-INIT-DCREA-DEB.
           MOVE WS-MVTS-DATE                   TO WS-CPTS-DCREA.
       7160-INIT-DCREA-FIN.
           EXIT.
      *
       7170-INIT-CPT-PRI-KEY-DEB.
           MOVE WS-MVTS-CPTE                   TO FS-CPT-CPTE.
           MOVE 1                              TO WS-NB-PAGE.
           MOVE 0                              TO WS-NB-LIGNE.
           SET  CLOTURE-FALSE                  TO TRUE.
           MOVE SPACES                         TO WS-LETAT-CLOSE-ED.
       7170-INIT-CPT-PRI-KEY-FIN.
           EXIT.
      *
       7999-INIT-DATE-DEB.
           ACCEPT WS-DATE                      FROM DATE YYYYMMDD.
           MOVE   WS-SS                        TO WS-LETAT-SS-ED
                                                  WS-L7-SS-ED.
           MOVE   WS-YY                        TO WS-LETAT-AA-ED
                                                  WS-L7-AA-ED
           MOVE   WS-MM                        TO WS-LETAT-MM-ED
                                                  WS-L7-MM-ED.
           MOVE   WS-DD                        TO WS-LETAT-JJ-ED
                                                  WS-L7-JJ-ED.
           MOVE 1                              TO WS-LETAT-PAGE-ED.
       7999-INIT-DATE-FIN.
           EXIT.
      *
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
       8000-EDIT-PDG-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6110-ECRIRE-ETATCLI-SDP-DEB
              THRU 6110-ECRIRE-ETATCLI-SDP-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L3                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L4                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN     2 TIMES.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L5                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L6                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L7                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L8                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
       8000-EDIT-PDG-ETATCLI-FIN.
           EXIT.
      *
      *
       8010-EDIT-PDG-ETATANO-DEB.
      *
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6130-ECRIRE-ETATANO-SDP-DEB
              THRU 6130-ECRIRE-ETATANO-SDP-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-ENTETE-L3              TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-ENTETE-L4              TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-ENTETE-L5              TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-ENTETE-L6              TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L7                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L8                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L2                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
      *
       8010-EDIT-PDG-ETATANO-FIN.
           EXIT.
      *
      *
       8020-EDIT-ET-DET-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6110-ECRIRE-ETATCLI-SDP-DEB
              THRU 6110-ECRIRE-ETATCLI-SDP-FIN.
      *
           MOVE WS-LETAT-DATE-PAGE             TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-NUMCPT                TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP               TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRES                TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
       8020-EDIT-ET-DET-ETATCLI-FIN.
           EXIT.
      *
      *
       8030-EDIT-DETAIL-ETATCLI-DEB.
      *
           MOVE WS-LETAT-DETAIL-OP             TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
       8030-EDIT-DETAIL-ETATCLI-FIN.
           EXIT.
      *
      *
       8040-EDIT-TOT-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-TOTCR                       TO WS-LETAT-TOTCR-ED.
           MOVE WS-TOTDB                       TO WS-LETAT-TOTDB-ED.
           MOVE WS-LETAT-TOT-OP                TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP               TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
       8040-EDIT-TOT-ETATCLI-FIN.
           EXIT.
      *
       8050-EDIT-ET-DET-ANO-DEB.
      *
           MOVE WS-LANO-L1                     TO WS-BUFFER.
           PERFORM 6130-ECRIRE-ETATANO-SDP-DEB
              THRU 6130-ECRIRE-ETATANO-SDP-FIN.
           MOVE WS-LANO-TITRES                 TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-L3                     TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
       8050-EDIT-ET-DET-ANO-FIN.
           EXIT.
       8060-EDIT-DET-ANO-DEB.
           MOVE WS-LANO-DETAIL                 TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
       8060-EDIT-DET-ANO-FIN.
           EXIT.
      *
      *
       8070-EDIT-TOT-ANO-DEB.
           MOVE WS-LANO-L3                     TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-TOT-ANO                     TO WS-LANO-TOTAL-ED.
           MOVE WS-LANO-TOTAL                  TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
           MOVE WS-LANO-L1                     TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
      *
       8070-EDIT-TOT-ANO-FIN.
           EXIT.
      *
       8080-EDIT-ANO-OK-DEB.
      *
           MOVE WS-LANO-OK                     TO WS-BUFFER.
           PERFORM 6140-ECRIRE-ETATANO-DEB
              THRU 6140-ECRIRE-ETATANO-FIN.
      *
       8080-EDIT-ANO-OK-FIN.
           EXIT.
      *
       8090-EDIT-PDP-ETATCLI-DEB.
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-LETAT-TOT-OP                TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L2B                  TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-LETAT-SOLD-OP               TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
           MOVE WS-ENTETE-L1                   TO WS-BUFFER.
           PERFORM 6120-ECRIRE-ETATCLI-DEB
              THRU 6120-ECRIRE-ETATCLI-FIN.
       8090-EDIT-PDP-ETATCLI-FIN.
           EXIT.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *
       8999-STATISTIQUES-DEB.
      *
           DISPLAY '************************************************'.
           DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO449        *'.
           DISPLAY '*     =================================        *'.
           DISPLAY '************************************************'.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
      * NOMBRE DE CLIENT
           DISPLAY WS-LCRE-CLIENT-ED.
      *
      * NOMBRE DE CLIENT NOUVEAUX.
           MOVE WS-CCLIN                       TO WS-LANO-CLINEW-TOT-ED.
           DISPLAY WS-LCRE-CLINEWF-ED.
      *
      * NOMBRE DE CLOTURE
           MOVE WS-CCLICLO                     TO WS-LANO-CLICLO-TOT-ED.
           DISPLAY WS-LCRE-CLICLO-ED.
      *
      * NOMBRE DE CLIENT STANDARD
           MOVE WS-CCLISTD                     TO WS-LANO-CLISTD-TOT-ED.
           DISPLAY WS-LCRE-CLISTDF-ED
      *
      * NOMBRE DE MOUVEMENT
           DISPLAY WS-LCRE-MVTS-ED.
      *
      * NOMBRE DE MOUVEMENT ERRONES.
           MOVE WS-CERR                        TO WS-LANO-MVTE-TOT-ED.
           DISPLAY WS-LCRE-MVTE-ED.
      *
      * NOMBRE DE RETRAIT.
           MOVE WS-CRET                        TO WS-LANO-RET-TOT-ED.
           DISPLAY WS-LCRE-RET-ED.
      *
      * NOMBRE DE CARTE BLEUES.
           MOVE WS-CCB                         TO WS-LANO-CBS-TOT-ED.
           DISPLAY WS-LCRE-CBS-ED.
      *
      * NOMBRE DE DEPOTS.
           MOVE WS-CDEP                        TO WS-LANO-DEP-TOT-ED.
           DISPLAY WS-LCRE-DEP-ED.
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO449         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO449        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
