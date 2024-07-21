      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0042.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0042.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 11:18:48   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Rest
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:29:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DPVB0042                                      * 
      *    DIESE SECTION ERMITTELT DIE SOG. VB1-ANSCHRIFT             * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 
      *    DATUM:    14.05.1993                                       * 
      ***************************************************************** 
      *    VERSION   02   01.10.93                                   *  
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !14.05.93!    ! NEU                       ! NAUMANN  *  
      *         !29.07.93!    ! S. TESTBERICHT TEST STUFE ! NAUMANN  *  
      *         !        !    ! I, 1. TEST, FEHL-NR 6/3   !          *  
      *         !17.09.93!    ! AENDERUNG LT. WARTUNGSAUFTRAG VOM    *  
      *         !        !    ! 16.09.93                  ! SCHMIDT  *  
      *         !16.09.93! 01 ! PROD.UEBERGABE            ! NAUMANN  *  
      *         !30.09.93!    ! GEÄNDERTE VERARBEITUNG DER ANSCHRIFT:*  
      *         !        !    ! BESSERE PERFORMANCE AUFGRUND WENIGER *  
      *         !        !    ! DB2-SELECTS AUF DIE MAAN! ! NAUMANN  *  
      *         !01.10.93! 02 ! PROD.-UEBERGABE           ! NAUMANN  *  
      * AVB01029!14.05.09!2.1 ! neue View VIVBA003        ! Walb     *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *---------------------------------------------------------------  
       VB1-ANSCHRIFT-VERARB SECTION.                                    
      *----------------------------------------------------------       
      *-INPUT : KNR-ST-TYP                                              
      *-        KEY-ID                                                  
      *-        MA-ST-NR IN DCL-VIVB0003                                
      *-OUTPUT: MSTRASSE                                                
      *-        MPLZ                                                    
      *-        MAUSL                                                   
      *-        MORT                                                    
      *-        MTELNR                                                  
      *-        MZUSATZKZ                                               
      *----------------------------------------------------------       
      *--NUR FÜR RFT, JUR, KL UND MA:                          --       
      *--UND FÜR ED (BEIM ED-KONTO WIRD DER ED-BESITZER GENOM- --       
      *--MEN)                                                  --       
      *--R 3.0.00:                                             --       
      *--IN FOLGENDER REIHENFOLGE DES ZUSTELLUNGSTYPEN WIRD DIE--       
      *--MAAN GELESEN:                                         --       
      *--KNR-ZUSTELLUNG:                                       --       
      *--      11                                              --       
      *--      22                                              --       
      *--      01                                              --       
      *--      02                                              --       
      *--      12                                              --       
      *--      21                                              --       
      *----------------------------------------------------------       
       VB1-ANSCHRIFT-VERARB-ANF.                                        
      *----------------------------------------------------------       
           EVALUATE KNR-ST-TYP                                          
           WHEN     K-KNR-ED                                            
           WHEN     K-KNR-MA                                            
           WHEN     K-KNR-KL                                            
           WHEN     K-KNR-RFT                                           
           WHEN     K-KNR-JPI                                           
              IF KNR-ST-TYP = K-KNR-ED                                  
140509*          MOVE MA-ST-NR IN DCL-VIVB0003 TO Z-ST-NR               
140509           MOVE MA-ST-NR IN DCL-VIVBA003 TO Z-ST-NR               
              ELSE                                                      
                 MOVE KEY-ID                   TO Z-ST-NR               
              END-IF                                                    
                                                                        
300993        MOVE    KS-NOT-FOUND           TO Z-SQLCODE               
300993        PERFORM VARYING I-IND2 FROM 1 BY 1                        
300993           UNTIL        I-IND2    > K-ANZ-ANSCHR                  
300993           OR           Z-SQLCODE NOT = KS-NOT-FOUND              
300993           MOVE    Z-ST-NR               TO MAAN-BEZUGS-NR        
300993           MOVE    K-KNR-ANSCHR (I-IND2) TO KNR-MAAN-TYP          
300993           PERFORM MAAN-LESEN                                     
300993                                                                  
300993           EVALUATE   SQLCODE                                     
300993           WHEN       KS-OK                                       
300993              PERFORM VARYING I-IND1 FROM 1 BY 1                  
300993                 UNTIL        I-IND1     > K-ANZ-ZUSTELL-VB1      
300993                 IF K-KNR-ZUSTELLUNG-VB1 (I-IND1) = KNR-ZUSTELLUNG
300993                    EVALUATE KNR-GRND                             
300993                       WHEN K-KNR-GUELTIGE-ANSCHR                 
300993                       WHEN K-KNR-ANSCHR-KUENDIGUNG               
300993                          PERFORM VB1-ANSCHRIFT-BELEGEN           
300993                       WHEN K-KNR-UNBEKANNT-VERZOGEN              
300993                       WHEN K-KNR-ANNAHME-VERWEIGERT              
300993                       WHEN K-KNR-ANSCHR-VERSTORBEN               
300993                       WHEN K-KNR-ANSCHR-BEENDET                  
300993                          PERFORM ANSCHRIFT-UNGUELT-BELEGEN       
300993                    END-EVALUATE                                  
300993                    PERFORM VB1-ANSCHRIFT-FUELLEN                 
300993                    MOVE SQLCODE         TO Z-SQLCODE             
300993                 END-IF                                           
300993              END-PERFORM                                         
300993           WHEN       KS-NOT-FOUND                                
300993              MOVE SQLCODE         TO Z-SQLCODE                   
300993           WHEN       OTHER                                       
300993                 SET VERARB-SQL-CODE-EX    TO TRUE                
300993                 MOVE SQLCODE              TO WLR-SQL-CODE        
300993                                              Z-SQLCODE           
300993                 IF    SQLERRML       >   ZERO                    
300993                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT   
300993                  ELSE                                            
300993                      MOVE  SPACE          TO   WLR-FEHLER-TEXT   
300993                 END-IF                                           
300993           END-EVALUATE                                           
300993        END-PERFORM                                               
300993*       MOVE    KS-NOT-FOUND TO Z-SQLCODE                         
300993*       PERFORM VARYING I-IND1 FROM 1 BY 1                        
300993*          UNTIL        I-IND1     > K-ANZ-ZUSTELL-VB1            
300993*             OR        Z-SQLCODE  NOT = KS-NOT-FOUND             
300993*          MOVE       Z-ST-NR       TO MAAN-BEZUGS-NR             
300993*          MOVE       K-KNR-ZUSTELLUNG-VB1 (I-IND1) TO            
300993*                     KNR-ZUSTELLUNG                              
300993*          PERFORM    MAAN-LESEN                                  
300993*                                                                 
300993*          EVALUATE   SQLCODE                                     
300993*          WHEN       KS-OK                                       
300993*                                                                 
300993*             EVALUATE KNR-GRND                                   
300993*                WHEN K-KNR-GUELTIGE-ANSCHR                       
300993*                WHEN K-KNR-ANSCHR-KUENDIGUNG                     
300993*                   PERFORM VB1-ANSCHRIFT-BELEGEN                 
300993*                WHEN K-KNR-UNBEKANNT-VERZOGEN                    
300993*                WHEN K-KNR-ANNAHME-VERWEIGERT                    
300993*                WHEN K-KNR-ANSCHR-VERSTORBEN                     
300993*                WHEN K-KNR-ANSCHR-BEENDET                        
300993*                   PERFORM ANSCHRIFT-UNGUELT-BELEGEN             
300993*             END-EVALUATE                                        
300993*                                                                 
300993*             PERFORM VB1-ANSCHRIFT-FUELLEN                       
300993*             MOVE SQLCODE         TO Z-SQLCODE                   
300993*          WHEN       KS-NOT-FOUND                                
300993*             MOVE SQLCODE         TO Z-SQLCODE                   
300993*          WHEN       OTHER                                       
300993*                SET VERARB-SQL-CODE-EX    TO TRUE                
300993*                MOVE SQLCODE              TO WLR-SQL-CODE        
300993*                                             Z-SQLCODE           
300993*                IF    SQLERRML       >   ZERO                    
300993*                    MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT   
300993*                 ELSE                                            
300993*                     MOVE  SPACE          TO   WLR-FEHLER-TEXT   
300993*                END-IF                                           
300993*          END-EVALUATE                                           
300993*       END-PERFORM                                               
                                                                        
           WHEN     K-KNR-SDNR                                          
           WHEN     K-KNR-GK                                            
               CONTINUE                                                 
           END-EVALUATE                                                 
           .                                                            
       VB1-ANSCHRIFT-VERARB-EXIT.                                       
           EXIT.                                                        
