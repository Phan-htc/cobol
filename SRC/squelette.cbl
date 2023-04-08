      *================================================================*
      *--                        INFORMATION GENERAL                 --*
      *----------------------------------------------------------------*
      *  NOM DU PROGRAMME : BASE                                       *
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
      *                     XXXXXXXX : FICHIER DES XXXXXXX
      *                     ------------------------------------------
          SELECT  XXXXXXXXXXXX       ASSGIN TO XXXXXXXXXXXX
                  FILE STATUS        IS XXXXXXXXXXXX.
      *                     ------------------------------------------
      *
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
       FD  XXXXXXXXXXXXX
           DATA RECORD IS XXXXXXXXXXXX.
       01  XXXXXXXXXXXX.
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================            
       77  WS-FS-XXXXXXXX    PIC(2).
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
      *6000-ORDRE-FICHIER-FIN.                                                               *
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