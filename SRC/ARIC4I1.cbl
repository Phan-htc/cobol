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
                     
      *    VARIABLE CODE RETOUR
                     
       77  WS-CICS-RC                      PIC S9(4)    COMP.
                     
      *    VARIABLE POUR MESSAGE NON FORMATE
                     
       77  WS-MSG-FIN                      PIC X(79).
                     
           88 MSG-FIN-PROG                              VALUE
                     
                 'FIN DE LA TRANSACTION'.
                     
      *              
              
      * VARIABLES DATE ET RESERVE
              
       77  WS-ABSTIME                      PIC X(15).

      *77  WS-DATE                         PIC X(10).

      *    05  WS-JJ                       PIC 99.

      *    05  FILLER                      PIC X        VALUE '.'.

      *    05  WS-MM                       PIC 99.

      *    05  FILLER                      PIC X        VALUE '.'.

      *    05  WS-AAAA                     PIC 9(4).

       77  WS-STATUT-TRANS                 PIC X.

           88  FIN-TRANS                                VALUE 'F'.

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

      *

      *---------------------------------------------------------------*

      * APPEL DU COMPOSANT SUIVANT (AS)

      *---------------------------------------------------------------*

           IF EIBCALEN = 0

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

                 THRU 9999-FIN-PROGRAMME-FIN.

           ELSE

              PERFORM 9999-RETURN-TRANSID-DEB

                 THRU 9999-RETURN-TRANSID-FIN.

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

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

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

      *---------------------------------------------------------------*

      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)

      *---------------------------------------------------------------*

           EVALUATE TRUE

              WHEN DFHENTER

                 PERFORM 2000-ENTREE-DEB

                    THRU 2000-ENTREE-FIN

              WHEN DFHPF3

                 PERFORM 2010-PF3-DEB

                    THRU 2010-PF3-FIN

              WHEN DFHCLEAR

                 PERFORM 2020-ALTC-DEB

                    THRU 2020-ALTC-FIN

              WHEN OTHER

                 PERFORM 2030-TFCT-WRONG-DEB

                    THRU 2030-TFCT-WRONG-FIN

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
                   
           IF WS-RC = DFHRESP(MAPFAIL)
                   
              PERFORM 3000-CHOIX-VIDE-DEB
                   
                 THRU 3000-CHOIX-VIDE-FIN
                   
           ELSE    
                   
              PERFORM 3010-CHOIX-FAIT-DEB
                   
                 THRU 3010-CHOIX-FAIT-FIN

           END-IF.

      *

      *---------------------------------------------------------------*

      * FIN DU TRAITEMENT (OREILLETTE DROITE)

      *---------------------------------------------------------------*

       2000-ENTREE-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      *               DESCRIPTION DU COMPOSANT PF3                    *

      *               ============================                    *

      *---------------------------------------------------------------*

      * DESCRIPTION                                                   *

      *---------------------------------------------------------------*

       2010-PF3-DEB.

      *---------------------------------------------------------------*

      * TRAITEMENT DE PLUS BAS NIVEAU

      *---------------------------------------------------------------*

           PERFORM 7070-PREP-FINTRS-DEB

              THRU 7070-PREP-FINTRS-FIN.

      *

       2010-PF3-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      *               DESCRIPTION DU COMPOSANT ALTC                   *

      *               =============================                   *

      *---------------------------------------------------------------*

      * DESCRIPTION                                                   *

      *---------------------------------------------------------------*

       2020-ALTC-DEB.

      *---------------------------------------------------------------*

      * TRAITEMENT DE PLUS BAS NIVEAU

      *---------------------------------------------------------------*

           PERFORM 7000-INIT-MAP-DEB

              THRU 7000-INIT-MAP-FIN.

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

           PERFORM 7020-PREP-MSG-CLEAR-DEB

              THRU 7020-PREP-MSG-CLEAR-FIN.

      *

           PERFORM 6000-SEND-FULLMAP-DEB

              THRU 6000-SEND-FULLMAP-FIN.

      *

       2020-ALTC-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      *            DESCRIPTION DU COMPOSANT T-FCT-WRONG               *

      *            ====================================               *

      *---------------------------------------------------------------*

      * DESCRIPTION                                                   *

      *---------------------------------------------------------------*

       2030-TFCT-WRONG-DEB.

      *---------------------------------------------------------------*

      * TRAITEMENT DE PLUS BAS NIVEAU

      *---------------------------------------------------------------*

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

           PERFORM 7030-PREP-MSG-TFCT-WRONG-DEB

              THRU 7030-PREP-MSG-TFCT-WRONG-FIN.

      *

           PERFORM 6010-SEND-LMAP-DEB

              THRU 6010-SEND-LMAP-FIN.

      *

       2030-TFCT-WRONG-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      *            DESCRIPTION DU COMPOSANT CHOIX-VIDE                *

      *            ===================================                *

      *---------------------------------------------------------------*

      * DESCRIPTION                                                   *

      *---------------------------------------------------------------*

       3000-CHOIX-VIDE-DEB.

      *

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

           PERFORM 7040-PREP-MSG-NOCHOICE-DEB

              THRU 7040-PREP-MSG-NOCHOICE-FIN.

      *

           PERFORM 6010-SEND-LMAP-DEB

              THRU 6010-SEND-LMAP-FIN.

      *

      *---------------------------------------------------------------*

      * TRAITEMENT DE PLUS BAS NIVEAU

      *---------------------------------------------------------------*

      *

       3000-CHOIX-VIDE-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      *               DESCRIPTION DU COMPOSANT CHOIX-FAIT             *

      *               ===================================             *

      *---------------------------------------------------------------*

      * DESCRIPTION                                                   *

      *---------------------------------------------------------------*

       3010-CHOIX-FAIT-DEB.

      *

      *---------------------------------------------------------------*

      * DEBUT DU TRAITEMENT (OREILLETTE GAUCHE)

      *---------------------------------------------------------------*

      *---------------------------------------------------------------*

      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)

      *---------------------------------------------------------------*

           EVALUATE TRUE

              WHEN CHOIX-OK

                 PERFORM 4010-OPT-OK-DEB

                    THRU 4010-OPT-OK-FIN

              WHEN OTHER

                 PERFORM 4000-OPT-WRONG-DEB

                    THRU 4000-OPT-WRONG-FIN

           END-EVALUATE.

      *

      *---------------------------------------------------------------*

      * FIN DU TRAITEMENT (OREILLETTE DROITE)

      *---------------------------------------------------------------*

       3010-CHOIX-FAIT-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      * OPT-WRONG (TRAITEMENT DE PLUS BAS NIVEAU)                     *

      *                                                               *

      *---------------------------------------------------------------*

       4000-OPT-WRONG-DEB.

      *

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

           PERFORM 7050-PREP-MSG-OPT-WRONG-DEB

              THRU 7050-PREP-MSG-OPT-WRONG-FIN.

      *

           PERFORM 6010-SEND-LMAP-DEB

              THRU 6010-SEND-LMAP-FIN.

      *

       4000-OPT-WRONG-FIN.

           EXIT.

      *

      *---------------------------------------------------------------*

      * OPT-OK (TRAITEMENT DE PLUS BAS NIVEAU)                        *

      *                                                               *

      *---------------------------------------------------------------*

       4010-OPT-OK-DEB.

      *

           PERFORM 7010-PREP-DATE-TSK-DEB

              THRU 7010-PREP-DATE-TSK-FIN.

           PERFORM 7060-PREP-MSG-CHOIX-DEB

              THRU 7060-PREP-MSG-CHOIX-FIN.

      *

           PERFORM 6010-SEND-LMAP-DEB

              THRU 6010-SEND-LMAP-FIN.

      *

       4010-OPT-OK-FIN.

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

                SEND MAP (ARIM461)

                MAPSET   (ARIN461)

                ERASE

                CURSOR

                RESP     (WS-RC)

           END-EXEC.

      *

           IF WS-RC NOT = DFHRESP(NORMAL)

              MOVE 'ERREUR ENVOI MAP COMPLETE' TO WS-MSG-FIN

              PERFORM 9999-ERREUR-PROGRAMME-DEB

                 THRU 9999-ERREUR-PROGRAMME-FIN

           END-IF.

       6000-SEND-FULLMAP-FIN.

           EXIT.

      *

       6010-SEND-LMAP-DEB.

           EXEC CICS

                SEND MAP (ARIM461)

                MAPSET   (ARIN461)

                DATAONLY

                RESP     (WS-RC)

           END-EXEC.

      *

           IF WS-RC NOT = DFHRESP(NORMAL)

              MOVE 'ERREUR ENVOI MAP LOGIQUE' TO WS-MSG-FIN

              PERFORM 9999-ERREUR-PROGRAMME-DEB

                 THRU 9999-ERREUR-PROGRAMME-FIN

           END-IF.

       6010-SEND-LMAP-FIN.

           EXIT.

      *

       6020-RECIEVE-MAP-DEB.

           EXEC CICS

                RECIEVE MAP (ARIM461)

                MAPSET      (ARIN461)

                INTO        (ARIM461O)

                RESP        (WS-RC)

           END-EXEC.

      *

           IF NOT (WS-RC = DFHRESP(NORMAL)

                   OR WS-RC = DFHRESP(MAPFAIL))

              MOVE 'ERREUR RECEPTION MAP' TO WS-MSG-FIN

              PERFORM 9999-ERREUR-PROGRAMME-DEB

                 THRU 9999-ERREUR-PROGRAMME-FIN

           END-IF.

       6020-RECIEVE-MAP-FIN.

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

      *

       7000-INIT-MAP-DEB.                           

      * INITIALISATION VIDE DE LA MAP LOGIQUE       

           MOVE LOW-VALUE          TO ARIM461O.     

      * POSITIONNEMENT DU CURSEUR                   

           MOVE -1                 TO MCHOIXL.      

      * ASSIGNATION NUM. TERMINAL ET ID TRANSACTION 

           MOVE EIBTRNID           TO MTRANI.       

           MOVE EIBTRMID           TO MTERMI.       

       7000-INIT-MAP-FIN.                           

           EXIT.                                    

      *

       7010-PREP-DATE-TSK-DEB.               

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

           MOVE EIBTASKN         TO MTASKI.  

      *                                      

       7010-PREP-DATE-TSK-FIN.               

           EXIT.                             

      *                                      

       7020-PREP-MSG-CLEAR-DEB.              

           MOVE WS-MSG(2)        TO MMSGO.   

       7020-PREP-MSG-CLEAR-FIN.              

           EXIT.                             

      *                                      

       7030-PREP-TFCT-WRONG-DEB.             

           MOVE WS-MSG(1)        TO MMSGO.   

       7030-PREP-TFCT-WRONG-FIN.             

           EXIT.                             

      *                                      

       7040-PREP-MSG-NOCHOICE-DEB.                      

           MOVE WS-MSG(24)       TO MMSGO.              

       7040-PREP-MSG-NOCHOICE-FIN.                      

           EXIT.                                        

      *                                                 

       7050-PREP-MSG-OPT-WRONG-DEB.                     

           MOVE WS-MSG(25)       TO MMSGO.              

       7050-PREP-MSG-OPT-WRONG-FIN.                     

           EXIT.                                        

      *                                                 

       7060-PREP-MSG-CHOIX-DEB.                         

           STRING 'VOUS AVEZ CHOISI L''OPTION ' MCHOIXO 

             INTO MMSGO                                 

           END-STRING.                                  

       7060-PREP-MSG-CHOIX-FIN.                         

           EXIT.                                        

      *                                      

       7070-PREP-FINTRS-DEB.                    

           SET  FIN-TRANS        TO TRUE.       

           MOVE WS-MSG(26)       TO WS-MSG-FIN. 

       7070-PREP-FINTRS-FIN.                    

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
                                     
      *     DISPLAY '*==============================================*'.  
                                     
      *     DISPLAY '*     FIN NORMALE DU PROGRAMME ARIC461         *'.  
                                     
      *     DISPLAY '*==============================================*'.  
                                     
      *                                                                  

       9999-FIN-PROGRAMME-FIN.                                           

            EXIT.

      *

       9999-RETURN-TRANSID-DEB.          

      * FIN TEMPORAIRE DU PROGRAMME      
                                     
           EXEC CICS                     
                                     
                RETURN                   
                                     
                TRANSID   (     )        
                                     
                COMMAREA  (WS-COMMAREA)  
                                     
                RESP      (WS-CICS-RC)   
                                     
           END-EXEC.                     
                                     
       9999-RETURN-TRANSID-FIN.          
                                     
           EXIT.                         
                                     
      *                                  
                                     
       9999-ERREUR-PROGRAMME-DEB.    
                                     
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
                                     
      *     DISPLAY '*==============================================*'.
                                     
      *     DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
                                     
      *     DISPLAY '*     FIN ANORMALE DU PROGRAMME ARID446        *'.

      *     DISPLAY '*==============================================*'.

      *     MOVE 12 TO RETURN-CODE.

      *

       9999-ERREUR-PROGRAMME-FIN.

            STOP RUN.
