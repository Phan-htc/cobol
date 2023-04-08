      *================================================================*
      *--                        INFORMATION GENERAL                 --*
      *----------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO249                                       *
      *  NOM DU REDACTEUR : MOI                                        *
      *  SOCIETE          : CHEZ MOI                                   *
      *  DATE DE CREATION : 05/02/2023                                 *
      *----------------------------------------------------------------*
      *--                  OBJECTIFS GENERAUX DU PROGRAMME           --*
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *                     HISTORIQUE DES MODIFICATIONS               *
      *----------------------------------------------------------------*
      * DATE  MODIF    !             NATURE DE LA MODIFICATION
      *----------------------------------------------------------------*
      *                !                                               *
      *================================================================*
      *
      ******************************
       IDENTIFICATION DIVISION.
      ******************************
       PROGRAME-ID.           BASE.
      *
      *                     ==============================             *
      *====================<  ENVIRONNEMENT    DIVISION   >============*
      *                     ==============================             *
      *
      **********************
       environment division.
      **********************
      *
      *======================
       CONFIGURATION SECTION.
      *====================== 
      *
      *______________
       SPECIAL-NAMES.
      *______________
           DECIMAL-POINT IS COMMA.
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *_____________
       FILE-CONTROL.
      *_____________
      *
      *                     ------------------------------------------
      *                     MVTS : FICHIER DES MOUVEMENTS
      *                     ------------------------------------------
          SELECT  MVTS        ASSGIN TO INP001
                  FILE STATUS        IS WS-FS-MVTS-E.
      *                     ------------------------------------------
      *
      *                     ------------------------------------------
      *                     ETAT : FICHIER DES ETAT CLIENT
      *                     ------------------------------------------
          SELECT  ETAT        ASSGIN TO ETATCLI
                  FILE STATUS        IS WS-FS-ETATCLI-S.
      *                     ------------------------------------------
      *
      *                     ------------------------------------------
      *                     ANO : FICHIER DES ANOMALIES
      *                     ------------------------------------------
          SELECT  ANO         ASSGIN TO ETATANO
                  FILE STATUS        IS WS-FS-ETATANO-S.
      *                     ------------------------------------------
      *
      *                     ==============================             *
      *====================<       DATA    DIVISION       >============*
      *                     ==============================             *
      *                                                                *
      *================================================================*
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *=====================
       FILE SECTION.
      *=====================
      *
       FD  F-MVTS-E
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-F-MVTS.
       
       FD  F-ETAT-S
           RECORD CONTAINS 80 CHARACTERS.
       01  FS-ETATCLI-S.

       FD  F-ETATANO-S
           RECORD CONTAINS 80 CHARACTERS.
       01  FS-ETATANO-S.
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================            
       01  WS-FS-MVTS-E    PIC XX.
      *                                                                *
      *declaration des variables du fichier MVTS
      *voir comment déclarer les autres fichiers
      *                                                                *

       01  WS-ENRG-MVTS-E
           05 WS-MVTS-CPT                PIC 9(10).
           05 WS-MVTS-DATE
              10 WS-MVTS-ANNEE
                 15 WS-MVTS-SS           PIC 99.
                 15 WS-MVTS-AA           PIC 99.
              10 WS-MVTS-MM              PIC 99.
              10 WS-MVTS-JJ              PIC 99.
           05 WS-MVTS-CODE               PIC X.
           05 WS-MVTS-MT                 PIC X(21).
      *    
      01   WS-FS-ETATCLI-S 
           
           
      01   WS-FS-ETATANO-S
            
      *    
      *    
      *    
      *                     ==============================             *
      *====================<  PROCEDURE    DIVISION       >============*
      *                     ==============================             *
      *    
       PROCEDURE DIVISION.
      *
      *================================================================*
      *
      *================================================================*
      *       STRUCTURE DE LA PARTIE ALGOTITHMIQUE DU PROGRAMME        *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *-----------------DESCRIPTION DU COMPOSANT PROGRAMME-------------*
      *                 ==================================             *
      *----------------------------------------------------------------*
      *    
      *    1 : LES COMPOSANTS DU DIAGRAMMES SONT CODES A L'AIDE DE
      *        DEUX PARAGRAPHES XXXX-COMPOSANT-DEB
      *                         XXYY-COMPOSANT-FIN  
      *    
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE
      *        yy DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU
      *                                                                *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UNE CARTOUCHE DE 
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT                   
      *                                                                *
      *                                                                *
      *================================================================*
      *================================================================*
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *              DESCRIPTION DU COMPOSANT PROGRAMME                *
      *             ==================================                 *
      *----------------------------------------------------------------*        
      *
       0000-PROGRAMME-DEB.
      *
      *
      *
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN.
      *
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.                                     
      *
       0000-PROGRAMME-FIN
            EXIT.
      *
      *================================================================*
      *================================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME        *
      *----------------------------------------------------------------*
      *                                                                *
      *   6XXX- : ORDRES DE MANIPULATION DES FICHIERS                  *
      *   7XXX- : TRANSFERTS DE CALCULS COMPLEXES                      *
      *   8XXX- : ORDRES DE MANIPULATION DES EDITIONS                  *
      *   9XXX- : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES           *
      *   9999- : PROTECTION FIN DE PROGRAMME                          *
      *                                                                *
      *================================================================*
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
      *   6XXX- : ORDRES DE MANIPULATION DES FICHIERS                  *
      *----------------------------------------------------------------*
      *                                                                *
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.                                                  
      *    EXIT.
      *                                                                *
      *----------------------------------------------------------------*
      *   7XXX- : TRANSFERTS ET CALCULS COMPLEXES                      *
      *----------------------------------------------------------------*
       *                                                                *
      *7000-ORDRE-CALCUL-DEB.
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.     
      *                                                                *
      *----------------------------------------------------------------*
      *   8XXX- : ORDRES DE MANIPULATION D EDITIONS                    *
      *----------------------------------------------------------------*
      *                                                                *
      *8000-ORDRE-EDITION-DEB.
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *
       8999-STATISTIQUES-DEB.
      *
            DISPLAY '*************************************************'
            DISPLAY '*    STATISTIQUE DU PROGRAMME XXXXXXXX          *'
            DISPLAY '*    =================================          *'
            DISPLAY '*************************************************'.
      *
       8999-STATISTIQUES-FIN.
            EXIT.
      *
      *----------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATIONS DES SOUS-PROGRAMMES         *  
      *----------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *     EXIT.
      *
      *----------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                         *
      *----------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.

            DISPLAY '*===============================================*'.
            DISPLAY '*      FIN DU PROGRAMME XXXXXXXXX           ====*'.
            DISPLAY '*===============================================*'.   
       9999-FIN-PROGRAMME-FIN.
            STOP RUN.
      * le stop run doit changer de position ( il doit figurer dans la 
      * procedure)
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*===============================================*'.
            DISPLAY '*      UNE ANOMALIE A ETE DETECTEE          ====*'.
            DISPLAY '*   FIN ANORMALE DU PROGRAMME xxxxxxxxx         *'.
            DISPLAY '*===============================================*'.   
            MOVE 12 TO RETURN CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.