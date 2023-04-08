      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO549                                   *
      *  NOM DU REDACTEUR : PHAN                                      *
      *  SOCIETE          : TRICOPHO                                  *
      *  DATE DE CREATION : --/--/----                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * EDITER UNE LISTE DES COMPTES DES CLIENTS PAR PAGE DE CLES     *
      *                                                               *
      *                                                               *
      * EDITER DEUX FICHIERS EN FIN DE PROGRAMME :                    *
      *                                                               *
      * - ETATCLI : AFFICHE A CHAQUE PAGE UNE LISTE DE COMPTE VIA     *
      *             UNE CLE PRIMAIRE (NUMERO DU COMPTE) OU SECONDAIRE *
      *             (NOM DU CLIENT).                                  *
      *                                                               *
      * - ETATANO : AFFICHE TOUTES LES DEMANDES AYANT UN CODE AUTRE   *
      *             QUE A OU B AVEC LE NUMERO DE L'ERREUR, SON TYPE   *
      *             ET LE CONTENU DE L'ENREGISTREMENT                 *
      *                                                               *
      * EDITERA EN FIN DE PROGRAMME UN COMPTE RENDU D'EXECUTION POUR  *
      * L'EQUIPE D'EXPLOITATION.                                      *
      * CELUI-CI INDIQUERA LE NOMBRE DE DEMANDE ET LE NOMBRE DE       *
      * DEMANDE ERRONEES.                                             *
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
       PROGRAM-ID.      ARIO549.
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
      *                      CPTE : FICHIER DES COMPTES CLIENT
      *                      -------------------------------------------
           SELECT  F-CPTE-E             ASSIGN TO INP001
                   ORGANIZATION         IS INDEXED
                   ACCESS MODE          IS DYNAMIC
                   RECORD KEY           IS FS-CPTE-CPTE
                   ALTERNATE RECORD KEY IS FS-CPTE-NOM
                                        WITH DUPLICATES
                   FILE STATUS          IS WS-FS-CPTE-E.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER ETAT CLIENT
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S          ASSIGN TO ETATCLI
                   FILE STATUS          IS WS-FS-ETATCLI-S.
      *                      -------------------------------------------
      *
      *                      -------------------------------------------
      *                      ETATANO : FICHIER DES ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S          ASSIGN TO ETATANO
                   FILE STATUS          IS WS-FS-ETATANO-S.
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
       FD  F-CPTE-E
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-CPTE.
           05  FS-CPTE-CPTE             PIC X(10).
           05  FS-CPTE-NOM              PIC X(14).
           05  FILLER                   PIC X(26).
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
       COPY TP5LEDIT.
      *
       COPY TP5CPTE.
      *
       COPY TP5DEMAN.
      *
      * FILES STATUS
      *-------------
       77  WS-FS-CPTE-E                 PIC X(2).
           88 CE-SUCCES                 VALUE '00'.
           88 CE-EOF                    VALUE '10'.
           88 CE-KEY-NOT-FOUND          VALUE '23'.
           88 CE-READ-OK                VALUE '00'
                                              '02'
                                              '10'.
           88 CE-OP-OK                  VALUE '00'.
           88 CE-CHECK-OK               VALUE '00'
                                              '23'.
      *
       77  WS-FS-ETATCLI-S              PIC X(2).
           88 EC-SUCCES                 VALUE '00'.
      *
       77  WS-FS-ETATANO-S              PIC X(2).
           88 EA-SUCCES                 VALUE '00'.
      *--------------------------
      * VARIABLES FICHIER EXTERNE
      *--------------------------
      *
       77  WS-BUFFER                    PIC X(80).
      *
      * CUMUL POUR LE COMPTE RENDU D EXECUTION
      *
       77  WS-CERR                      PIC S9(4) COMP
                                        VALUE 0.
           88  CERR-ZERO                VALUE 0.
       77  WS-CDEM                      PIC S9(4) COMP
                                        VALUE 0.
       77  WS-CPT-LOW                   PIC 9(10).
       77  WS-NOM-LOW                   PIC X(20).
      *
      * N.CONDITION POUR LES TESTS
       77  WS-ETAT-NO-OK                PIC 9.
           88 ETAT-NO-OK                VALUE 1.
           88 ETAT-OK                   VALUE 0.
       77  WS-DEM-EMPTY                 PIC 9.
           88 DEM-EMPTY                 VALUE 0.
       77  WS-CLOSE                     PIC 9.
           88 CLOSE-CPT                 VALUE 1.
           88 KEEP-CPT                  VALUE 0.
       77  WS-CODE-ERR                  PIC 99.
           88 CODE-1                    VALUE 01.
           88 CODE-2                    VALUE 02.
           88 CODE-3                    VALUE 03.
           88 CODE-4                    VALUE 04.
           88 CODE-5                    VALUE 05.
           88 CODE-6                    VALUE 06.
           88 CODE-7                    VALUE 07.
       77  WS-TEXT-ERR                  PIC X(49).
           88  TEXT-1                   VALUE
               'TYPE DE DEMANDE INCONNUE.'.
           88  TEXT-2                   VALUE
               'DEMANDE INCOMPLETE.'.
           88  TEXT-3                   VALUE
               'NOM DU DEMANDEUR NON ALPHABETIQUE.'.
           88  TEXT-4                   VALUE
               'NOM CLIENT NON ALPHABETIQUE.'.
           88  TEXT-5                   VALUE
               'NUMERO DU COMPTE NON NUMERIQUE.'.
           88  TEXT-6                   VALUE
               'ERREUR ORDRE DES BORNES.'.
           88  TEXT-7                   VALUE
               'BORNES HORS LIMITES.'.
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
           PERFORM 6000-OUVRIR-FCPTE-DEB
              THRU 6000-OUVRIR-FCPTE-FIN.
      *
           PERFORM 6010-OUVRIR-ETATCLI-DEB
              THRU 6010-OUVRIR-ETATCLI-FIN.
      *
           PERFORM 6020-OUVRIR-ETATANO-DEB
              THRU 6020-OUVRIR-ETATANO-FIN.
      *
           PERFORM 6110-POSITION-CLE-PRIM-DEB
              THRU 6110-POSITION-CLE-PRIM-FIN.
      *
           PERFORM 6130-LIRE-NEXT-DEB
              THRU 6130-LIRE-NEXT-FIN.
      *
           PERFORM 7000-LOW-CPTE-DEB
              THRU 7000-LOW-CPTE-FIN.
      *
           PERFORM 6120-POSITION-CLE-SEC-DEB
              THRU 6120-POSITION-CLE-SEC-FIN.
      *
           PERFORM 6130-LIRE-NEXT-DEB
              THRU 6130-LIRE-NEXT-FIN.
      *
           PERFORM 7130-LOW-NOM-DEB
              THRU 7130-LOW-NOM-FIN.
      *
           PERFORM 6030-LIRE-SYSIN-DEB
              THRU 6030-LIRE-SYSIN-FIN.
      *
      * ITERATIVE LVL 0000
      *
      * APPEL DU COMPOSANT SUIVANT JUSQU'A LA FIN DE LA SYSIN
           PERFORM 1000-TRT-DEMANDE-DEB
              THRU 1000-TRT-DEMANDE-FIN
             UNTIL FIN-SYSIN.
      *
      * TRAITEMENT DU COMPOSANT (OREILLETTE DROITE LVL 0000)
           IF WS-CERR > 0
              PERFORM 8060-EDIT-PDP-ETATANO-DEB
                 THRU 8060-EDIT-PDP-ETATANO-FIN
           END-IF.
      *
           PERFORM 8999-STATISTIQUES-DEB
              THRU 8999-STATISTIQUES-FIN.
      *
           PERFORM 6040-FERMER-CPTE-DEB
              THRU 6040-FERMER-CPTE-FIN.
      *
           PERFORM 6050-FERMER-ETATCLI-DEB
              THRU 6050-FERMER-ETATCLI-FIN.
      *
           PERFORM 6060-FERMER-ETATANO-DEB
              THRU 6060-FERMER-ETATANO-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
           STOP RUN.
      *
      * TRAITEMENT COMPTE
      *
       1000-TRT-DEMANDE-DEB.
      *
      * (OREILLETTE GAUCHE LVL 1000)
      *
           PERFORM 7010-PREP-TYP-DEM-DEB
              THRU 7010-PREP-TYP-DEM-FIN.
      *
      * (ALTERNATIVE MULTIPLE)
           EVALUATE TRUE
      *
               WHEN TYPE-A
                   PERFORM 2000-TRT-PRI-KEY-DEB
                      THRU 2000-TRT-PRI-KEY-FIN
      *
               WHEN TYPE-B
                   PERFORM 2010-TRT-SEC-KEY-DEB
                      THRU 2010-TRT-SEC-KEY-FIN
      *
               WHEN OTHER
                   PERFORM 2020-TRT-KEY-INCONNU-DEB
                      THRU 2020-TRT-KEY-INCONNU-FIN
           END-EVALUATE.
      *
      * (OREILLETTE DROITE LVL 1000)
      *
           PERFORM 6030-LIRE-SYSIN-DEB
              THRU 6030-LIRE-SYSIN-FIN.
      *
       1000-TRT-DEMANDE-FIN.
           EXIT.
      *
      * FIN TRAITEMENT DEMANDE
      * ---------------------
      *
      * DEMANDE DE TYPE A
      * -----------------
       2000-TRT-PRI-KEY-DEB.
      *
      * (OREILLETTE GAUCHE LVL 2000)
      *
           PERFORM 6110-POSITION-CLE-PRIM-DEB
              THRU 6110-POSITION-CLE-PRIM-FIN
      *
           EVALUATE TRUE
               WHEN NOM-DEM-EMPTY OR CPT-DEB-EMPTY OR CPT-FIN-EMPTY
                    PERFORM 7030-INCOMPLETE-DEB
                       THRU 7030-INCOMPLETE-FIN
      *
               WHEN WS-DEM-NOM IS NOT ALPHABETIC
                    PERFORM 7040-NOM-DEM-NN-ALPHA-DEB
                       THRU 7040-NOM-DEM-NN-ALPHA-FIN
      *
               WHEN WS-DEM-CPT-DEB IS NOT NUMERIC OR
                    WS-DEM-CPT-FIN IS NOT NUMERIC
                    PERFORM 7060-CPT-NN-NUM-DEB
                       THRU 7060-CPT-NN-NUM-FIN
      *
               WHEN WS-DEM-CPT-DEB > WS-DEM-CPT-FIN
                    PERFORM 7070-ERR-SENS-BORNES-DEB
                       THRU 7070-ERR-SENS-BORNES-FIN
      *
               WHEN WS-DEM-CPT-FIN < WS-CPT-LOW OR
                    CE-KEY-NOT-FOUND
      *
                    PERFORM 7080-HORS-LIMIT-DEB
                       THRU 7080-HORS-LIMIT-FIN
           END-EVALUATE.
      *
      * (ALTERNATIVE SIMPLE)
      *
           IF ETAT-NO-OK
              PERFORM 3020-DEMANDE-INVALIDE-DEB
                 THRU 3020-DEMANDE-INVALIDE-FIN
           ELSE
              PERFORM 3000-TRT-PAR-PRI-KEY-DEB
                 THRU 3000-TRT-PAR-PRI-KEY-FIN
           END-IF.
      *
       2000-TRT-PRI-KEY-FIN.
           EXIT.
      *
      * DEMANDE DE TYPE B
      *------------------
       2010-TRT-SEC-KEY-DEB.
      *
      * (OREILLETTE GAUCHE LVL 2010)
      *
           PERFORM 6120-POSITION-CLE-SEC-DEB
              THRU 6120-POSITION-CLE-SEC-FIN
      *
           EVALUATE TRUE
               WHEN NOM-DEM-EMPTY OR CLI-DEB-EMPTY OR CLI-FIN-EMPTY
                    PERFORM 7030-INCOMPLETE-DEB
                       THRU 7030-INCOMPLETE-FIN
      *
               WHEN WS-DEM-NOM IS NOT ALPHABETIC
                    PERFORM 7040-NOM-DEM-NN-ALPHA-DEB
                       THRU 7040-NOM-DEM-NN-ALPHA-FIN
      *
               WHEN WS-DEM-CLI-DEB IS NOT ALPHABETIC OR
                    WS-DEM-CLI-FIN IS NOT ALPHABETIC
                    PERFORM 7050-NOM-CLI-NN-ALPHA-DEB
                       THRU 7050-NOM-CLI-NN-ALPHA-FIN
      *
               WHEN WS-DEM-CLI-DEB > WS-DEM-CLI-FIN
                    PERFORM 7070-ERR-SENS-BORNES-DEB
                       THRU 7070-ERR-SENS-BORNES-FIN
      *
               WHEN WS-DEM-CLI-FIN < WS-NOM-LOW OR
                    CE-KEY-NOT-FOUND
                    PERFORM 7080-HORS-LIMIT-DEB
                    THRU 7080-HORS-LIMIT-FIN
           END-EVALUATE.
      * ALTERNATIVE SIMPLE
           IF ETAT-NO-OK
              PERFORM 3020-DEMANDE-INVALIDE-DEB
                 THRU 3020-DEMANDE-INVALIDE-FIN
           ELSE
              PERFORM 3010-TRT-PAR-SEC-KEY-DEB
                 THRU 3010-TRT-PAR-SEC-KEY-FIN
           END-IF.
      *
       2010-TRT-SEC-KEY-FIN.
           EXIT.
      *
       2020-TRT-KEY-INCONNU-DEB.
      *
           IF CERR-ZERO
              PERFORM 8020-EDIT-ENTETE-ANO-DEB
                 THRU 8020-EDIT-ENTETE-ANO-FIN
           END-IF.
           PERFORM 7020-ERR-TYP-DEB
              THRU 7020-ERR-TYP-FIN.
      *
           PERFORM 7120-PREPARE-DET-ANO-DEB
              THRU 7120-PREPARE-DET-ANO-FIN.
      *
           PERFORM 8030-EDIT-DET-ANO-DEB
              THRU 8030-EDIT-DET-ANO-FIN.
      *
       2020-TRT-KEY-INCONNU-FIN.
           EXIT.
      *
       3000-TRT-PAR-PRI-KEY-DEB.
      *
      * (OREILLETTE GAUCHE LVL 3000)
           PERFORM 7090-PREPARE-ENTETE-CLI-A-DEB
              THRU 7090-PREPARE-ENTETE-CLI-A-FIN.
           PERFORM 8000-EDIT-ENTETE-ETATCLI-DEB
              THRU 8000-EDIT-ENTETE-ETATCLI-FIN.
           PERFORM 6130-LIRE-NEXT-DEB
              THRU 6130-LIRE-NEXT-FIN.
      *
           PERFORM 4000-RESULTAT-DEB
              THRU 4000-RESULTAT-FIN
             UNTIL CE-EOF OR (WS-CPTE-CPTE > WS-DEM-CPT-FIN).
      *
      * (OREILLETTE DROITE LVL 3000)
      *
           IF DEM-EMPTY
              PERFORM 8040-ALERT-NO-CPT-DEB
                 THRU 8040-ALERT-NO-CPT-FIN
           END-IF.
           PERFORM 8050-EDIT-PDP-ETATCLI-DEB
              THRU 8050-EDIT-PDP-ETATCLI-FIN.
       3000-TRT-PAR-PRI-KEY-FIN.
           EXIT.
      *
       3010-TRT-PAR-SEC-KEY-DEB.
      *
      * (OREILLETTE GAUCHE LVL 3010)
           PERFORM 7100-PREPARE-ENTETE-CLI-B-DEB
              THRU 7100-PREPARE-ENTETE-CLI-B-FIN.
           PERFORM 8000-EDIT-ENTETE-ETATCLI-DEB
              THRU 8000-EDIT-ENTETE-ETATCLI-FIN.
           PERFORM 6130-LIRE-NEXT-DEB
              THRU 6130-LIRE-NEXT-FIN.
      *
           PERFORM 4000-RESULTAT-DEB
              THRU 4000-RESULTAT-FIN
             UNTIL CE-EOF OR (WS-CPTE-NOM > WS-DEM-CLI-FIN).
      *
      * (OREILLETTE DROITE LVL 3010)
           IF DEM-EMPTY
              PERFORM 8040-ALERT-NO-CPT-DEB
                 THRU 8040-ALERT-NO-CPT-FIN
           END-IF.
           PERFORM 8050-EDIT-PDP-ETATCLI-DEB
              THRU 8050-EDIT-PDP-ETATCLI-FIN.
       3010-TRT-PAR-SEC-KEY-FIN.
           EXIT.
      *
       3020-DEMANDE-INVALIDE-DEB.
      *
           IF WS-CERR = 0
              PERFORM 8020-EDIT-ENTETE-ANO-DEB
                 THRU 8020-EDIT-ENTETE-ANO-FIN
           END-IF.
           PERFORM 7120-PREPARE-DET-ANO-DEB
              THRU 7120-PREPARE-DET-ANO-FIN.
      *
           PERFORM 8030-EDIT-DET-ANO-DEB
              THRU 8030-EDIT-DET-ANO-FIN.
      *
       3020-DEMANDE-INVALIDE-FIN.
           EXIT.
      *
       4000-RESULTAT-DEB.
      *
           PERFORM 7110-PREPARE-DETAIL-CLI-DEB
              THRU 7110-PREPARE-DETAIL-CLI-FIN.
           PERFORM 8010-EDIT-DETAIL-ETATCLI-DEB
              THRU 8010-EDIT-DETAIL-ETATCLI-FIN.
           PERFORM 6130-LIRE-NEXT-DEB
              THRU 6130-LIRE-NEXT-FIN.
      *
       4000-RESULTAT-FIN.
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
      *
      *
       6000-OUVRIR-FCPTE-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT CE-SUCCES
              DISPLAY 'ERREUR OUVERTURE F-CPTE-E'
              DISPLAY 'CODE ERREUR : ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OUVRIR-FCPTE-FIN.
           EXIT.
      *
       6010-OUVRIR-ETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT EC-SUCCES
              DISPLAY 'ERREUR OUVERTURE F-ETATCLI-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6010-OUVRIR-ETATCLI-FIN.
           EXIT.
      *
       6020-OUVRIR-ETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR OUVERTURE F-ETATANO-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6020-OUVRIR-ETATANO-FIN.
           EXIT.
      *
       6030-LIRE-SYSIN-DEB.
      *
           ACCEPT WS-ENRG-SYSIN.
      *
       6030-LIRE-SYSIN-FIN.
           EXIT.
      *
       6040-FERMER-CPTE-DEB.
           CLOSE F-CPTE-E.
           IF NOT CE-SUCCES
              DISPLAY 'ERREUR FERMETURE F-CPTE-E'
              DISPLAY 'CODE ERREUR : ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6040-FERMER-CPTE-FIN.
           EXIT.
      *
       6050-FERMER-ETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT EC-SUCCES
              DISPLAY 'ERREUR FERMETURE F-ETATCLI-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6050-FERMER-ETATCLI-FIN.
           EXIT.
      *
       6060-FERMER-ETATANO-DEB.
           CLOSE F-ETATANO-S
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR FERMETURE F-ETATANO-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6060-FERMER-ETATANO-FIN.
           EXIT.
      *
       6070-ECRIRE-ETATCLI-SDP-DEB.
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
       6070-ECRIRE-ETATCLI-SDP-FIN.
           EXIT.
      *
       6080-ECRIRE-ETATCLI-DEB.
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
       6080-ECRIRE-ETATCLI-FIN.
           EXIT.
      *
       6090-ECRIRE-ETATANO-SDP-DEB.
           WRITE FS-ENRG-ETATANO
            FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATANO-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-ECRIRE-ETATANO-SDP-FIN.
           EXIT.
      *
       6100-ECRIRE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
            FROM WS-BUFFER
           END-WRITE.
           IF NOT EA-SUCCES
              DISPLAY 'ERREUR D ECRITURE ETATANO-S'
              DISPLAY 'CODE ERREUR : ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6100-ECRIRE-ETATANO-FIN.
           EXIT.
      *
       6110-POSITION-CLE-PRIM-DEB.
           MOVE WS-DEM-CPT-DEB                TO FS-CPTE-CPTE.
           START F-CPTE-E
                 KEY >= FS-CPTE-CPTE
           END-START.
           IF NOT CE-CHECK-OK
              DISPLAY 'ERREUR POINTEUR CLE PRINCIPALE'
              DISPLAY 'CODE RETOUR : ' WS-FS-CPTE-E
                   PERFORM 9999-ERREUR-PROGRAMME-DEB
                      THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6110-POSITION-CLE-PRIM-FIN.
           EXIT.
      *
       6120-POSITION-CLE-SEC-DEB.
           MOVE WS-DEM-CLI-DEB                 TO FS-CPTE-NOM.
           START F-CPTE-E
                 KEY >= FS-CPTE-NOM
           END-START.
           IF NOT CE-CHECK-OK
              DISPLAY 'ERREUR POINTEUR CLE SECONDAIRE'
              DISPLAY 'CODE RETOUR : ' WS-FS-CPTE-E
                   PERFORM 9999-ERREUR-PROGRAMME-DEB
                      THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6120-POSITION-CLE-SEC-FIN.
           EXIT.
      *
       6130-LIRE-NEXT-DEB.
           READ F-CPTE-E                NEXT
                INTO WS-ENRG-F-CPTE.
           IF NOT CE-READ-OK
              DISPLAY 'ERRREUR LECTURE F-CPTE-E'
              DISPLAY 'CODE RETOUR : ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-LIRE-NEXT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-LOW-CPTE-DEB.
      *
           MOVE WS-CPTE-CPTE                   TO WS-CPT-LOW.
      *
       7000-LOW-CPTE-FIN.
           EXIT.
      *
       7010-PREP-TYP-DEM-DEB.
           ADD  1                              TO WS-CDEM.
           MOVE WS-CDEM                        TO WS-LETAT-NUM-ED.
           MOVE 0                              TO WS-DEM-EMPTY.
           SET  ETAT-OK                        TO TRUE.
       7010-PREP-TYP-DEM-FIN.
           EXIT.
      *
       7020-ERR-TYP-DEB.
           SET  CODE-1                         TO TRUE.
           SET  TEXT-1                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7020-ERR-TYP-FIN.
           EXIT.
      *
       7030-INCOMPLETE-DEB.
           SET  CODE-2                         TO TRUE.
           SET  TEXT-2                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7030-INCOMPLETE-FIN.
           EXIT.
      *
       7040-NOM-DEM-NN-ALPHA-DEB.
           SET  CODE-3                         TO TRUE.
           SET  TEXT-3                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7040-NOM-DEM-NN-ALPHA-FIN.
           EXIT.
      *
       7050-NOM-CLI-NN-ALPHA-DEB.
           SET  CODE-4                         TO TRUE.
           SET  TEXT-4                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7050-NOM-CLI-NN-ALPHA-FIN.
           EXIT.
      *
       7060-CPT-NN-NUM-DEB.
           SET  CODE-5                         TO TRUE.
           SET  TEXT-5                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7060-CPT-NN-NUM-FIN.
           EXIT.
      *
       7070-ERR-SENS-BORNES-DEB.
           SET  CODE-6                         TO TRUE.
           SET  TEXT-6                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7070-ERR-SENS-BORNES-FIN.
           EXIT.
      *
       7080-HORS-LIMIT-DEB.
           SET  CODE-7                         TO TRUE.
           SET  TEXT-7                         TO TRUE.
           SET  ETAT-NO-OK                     TO TRUE.
       7080-HORS-LIMIT-FIN.
           EXIT.
      *
       7090-PREPARE-ENTETE-CLI-A-DEB.
      *
           MOVE WS-DEM-NOM                     TO WS-LETAT-NOMD-ED.
           MOVE WS-CDEM                        TO WS-LETAT-NUM-ED.
           MOVE 'NUM COMPTE CLIENT'            TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CPT-DEB                 TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CPT-FIN                 TO WS-LETAT-REFFIN-ED.
           MOVE 1                              TO WS-LETAT-PAGE-ED.
      *
       7090-PREPARE-ENTETE-CLI-A-FIN.
           EXIT.
      *
       7100-PREPARE-ENTETE-CLI-B-DEB.
      *
           MOVE WS-DEM-NOM                     TO WS-LETAT-NOMD-ED.
           MOVE WS-CDEM                        TO WS-LETAT-NUM-ED.
           MOVE 'NOM CLIENT'                   TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CLI-DEB                 TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CLI-FIN                 TO WS-LETAT-REFFIN-ED.
           MOVE 1                              TO WS-LETAT-PAGE-ED.
      *
       7100-PREPARE-ENTETE-CLI-B-FIN.
           EXIT.
      *
       7110-PREPARE-DETAIL-CLI-DEB.
      *
           MOVE WS-CPTE-NOM                    TO WS-LETAT-NOMC-ED.
           MOVE WS-CPTE-CPTE                   TO WS-LETAT-NUMCPT-ED.
           MOVE WS-CPTE-DCREA-SS               TO WS-LETAT-DCREA-SS-ED.
           MOVE WS-CPTE-DCREA-AA               TO WS-LETAT-DCREA-AA-ED.
           MOVE WS-CPTE-DCREA-MM               TO WS-LETAT-DCREA-MM-ED.
           MOVE WS-CPTE-DCREA-JJ               TO WS-LETAT-DCREA-JJ-ED.
      *
           MOVE WS-CPTE-DMAJ-SS                TO WS-LETAT-DMAJ-SS-ED.
           MOVE WS-CPTE-DMAJ-AA                TO WS-LETAT-DMAJ-AA-ED.
           MOVE WS-CPTE-DMAJ-MM                TO WS-LETAT-DMAJ-MM-ED.
           MOVE WS-CPTE-DMAJ-JJ                TO WS-LETAT-DMAJ-JJ-ED.
      *
           MOVE WS-CPTE-SOLDE                  TO WS-LETAT-SOLDE-ED.
           ADD  1                              TO WS-DEM-EMPTY.
      *
       7110-PREPARE-DETAIL-CLI-FIN.
           EXIT.
      *
       7120-PREPARE-DET-ANO-DEB.
      *
           ADD 1                               TO WS-CERR.
           MOVE WS-ENRG-SYSIN                  TO WS-LANO-ENR-ED.
           MOVE WS-CODE-ERR                    TO WS-LANO-NUM-ED.
           MOVE WS-TEXT-ERR                    TO WS-LANO-TYP-ED.
       7120-PREPARE-DET-ANO-FIN.
           EXIT.
      *
       7130-LOW-NOM-DEB.
      *
           MOVE WS-CPTE-NOM                    TO WS-NOM-LOW.
      *
       7130-LOW-NOM-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-EDIT-ENTETE-ETATCLI-DEB.
      *
           MOVE WS-LETAT-TIRET                 TO WS-BUFFER.
           PERFORM 6070-ECRIRE-ETATCLI-SDP-DEB
              THRU 6070-ECRIRE-ETATCLI-SDP-FIN.
      *
           MOVE WS-LETAT-ENTETE                TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC                 TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRE                 TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC                 TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-REFDEB                TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-REFFIN                TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC                 TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-INTITULE              TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
      *
       8000-EDIT-ENTETE-ETATCLI-FIN.
           EXIT.
      *
      *
       8010-EDIT-DETAIL-ETATCLI-DEB.
      *
           MOVE WS-LETAT-DETAIL                TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
       8010-EDIT-DETAIL-ETATCLI-FIN.
           EXIT.
      *
       8020-EDIT-ENTETE-ANO-DEB.
      *
           MOVE WS-LANO-ASTER                  TO WS-BUFFER.
           PERFORM 6090-ECRIRE-ETATANO-SDP-DEB
              THRU 6090-ECRIRE-ETATANO-SDP-FIN.
      *
           MOVE WS-LANO-TITRE                  TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
      *
           MOVE WS-LANO-ASTER                  TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
      *
       8020-EDIT-ENTETE-ANO-FIN.
           EXIT.
      *
       8030-EDIT-DET-ANO-DEB.
      *
           MOVE WS-LANO-ERREUR                 TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENR1                   TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENR2                   TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
      *
           MOVE WS-LANO-INTERL                 TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
       8030-EDIT-DET-ANO-FIN.
           EXIT.
      *
       8040-ALERT-NO-CPT-DEB.
           MOVE
           "!               AUCUN COMPTE SOUS CE NUMERO/NOM DE COMPTE
      -    "                 !"
                                               TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
       8040-ALERT-NO-CPT-FIN.
           EXIT.
      *
       8050-EDIT-PDP-ETATCLI-DEB.
           MOVE WS-LETAT-TIRET                 TO WS-BUFFER.
           PERFORM 6080-ECRIRE-ETATCLI-DEB
              THRU 6080-ECRIRE-ETATCLI-FIN.
       8050-EDIT-PDP-ETATCLI-FIN.
           EXIT.
      *
       8060-EDIT-PDP-ETATANO-DEB.
           MOVE WS-LANO-ASTER                  TO WS-BUFFER.
           PERFORM 6100-ECRIRE-ETATANO-DEB
              THRU 6100-ECRIRE-ETATANO-FIN.
       8060-EDIT-PDP-ETATANO-FIN.
           EXIT.
      *
      *
       8999-STATISTIQUES-DEB.
      *
           DISPLAY '************************************************'.
           DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO549        *'.
           DISPLAY '*     =================================        *'.
           DISPLAY '************************************************'.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           MOVE 'NOMBRE DE DEMANDES'           TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CDEM                        TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
      *
           MOVE 'NOMBRE DE DEMANDES ERRONEES'  TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CERR                        TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
      *
           DISPLAY WS-LCRE-ASTER.
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO549         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO549        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.
