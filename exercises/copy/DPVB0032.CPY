      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0032.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0032.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 11:18:28   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Rest
      *
      *   Rev 2.0   Feb 27 1999 15:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:27:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DPVB0032                                      * 
      *    DIESE SECTION ERMITTELT DIE ED-PROVISION EINES ED-KONTOS   * 
      *    ZU EINEM KONKRETEN ZEITPUNKT                               * 
      *    DESWEITEREN ERMITTELT ES DIE LD-PROVISION EINES LD'S       * 
      *    (KONKRET: DIE LD-PROVISION DES HERRN SCHUBERT UND NUR      * 
      *     DIESE) ZU EINEM KONKRETEN ZEITPUNKT                       * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 
      *    DATUM:    03.04.1993                                       * 
      ***************************************************************** 
      *    VERSION   2    01.10.94 08:25:20
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  
      *         !        !    !                           !          *  
      *         !24.08.93! 02 ! S. AKTENNOTIZ V. 16.8.93  ! NAUMANN  *  
      *         !        !    ! AN FR. WIEZOREK, PUNKT 3  !          *  
      *         !        !    !                           !          *  
      * AFVB0041!25.11.93! 03 ! SONDERFALL ED-KONTEN DES HERRN REI-  *  
      *         !        !    ! NARTZ                     ! NAUMANN  *  
      *         !        !    !                           !          *  
      * AFVB0150!07.09.94! 04 ! SONDERFALL ED-KONTO 8820600 DES HR.  *  
      *         !        !    ! STRANZIGNER, SIEHE AUCH AKTENNOTIZ   *  
      *         !        !    ! VON FR. WIEZOREK 05.09.94 ! SCHMIDT  *  
      *         !        !    !                           !          *  
      * AFVB01XX!20.09.94! 05 ! SONDERFALL ED-KONTEN 8520300, 8520110*  
      *         !        !    ! HR. REINARTZ SIEHE AUCH AKTENNOTIZ   *  
      *         !        !    ! VON FR. WIEZOREK 19.09.94 ! SCHMIDT  *  
      * AVB01029!11.05.09!2.1 ! ED-Prov. in Tabelle ED an-! Walb     *  
      *         !        !    ! statt in MAT              !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       ED-BETR-AUSPROV-ERMITT SECTION.                                  
      *-------------------------------------------------------------    
      * ED-BETR-AUSPROV-ERMITT                                     *    
      *------------------------------------------------------------*    
      * INPUT:  Z-ZTPKT-DATE                                       *    
      *         WB-MAT-ED-PROV1        IN WB-MAT-BETR              *    
      *         WB-NI-MAT-ED-PROV1     IN WB-MAT-BETR              *    
      *         WB-MAT-ED-PROV2        IN WB-MAT-BETR              *    
      *         WB-NI-MAT-ED-PROV2     IN WB-MAT-BETR              *    
      *         WB-MAT-ED-PROV-DAT1    IN WB-MAT-BETR              *    
      *         WB-NI-MAT-ED-PROV-DAT1 IN WB-MAT-BETR              *    
      *         WB-MAT-ED-PROV-DAT2    IN WB-MAT-BETR              *    
      *         WB-NI-MAT-ED-PROV-DAT2 IN WB-MAT-BETR              *    
241193*         WK-ED-ST-NR-BETR                                   *    
      *                                                            *    
      * OUTPUT: Z-AUSPROV                                          *    
      *                                                            *    
      * VERARBEITUNG: SIEHE OBEN                                   *    
      * GETROFFENE ANNAHMEN:                                       *    
      * 1.BEI AUFRUF DIESER SECTION MUß IMMER WB-MAT-ED-PROV-DAT1  *    
      *   GEFÜLLT SEIN, DENN DIES IST DIE AKTUELLE ED-PROVISION DES*    
      *   ED-BESITZERS.                                            *    
      * 2.DIE SECTION ERZEUGT EINEN LOG. FEHLER, WENN ZUM ANGEGE-  *    
      *   BENEN ZEITPUNKT KEINE GÜLTIGE ED-PROVISION EXISTIERTE.   *    
      *   -                                                        *    
      *   -                                                        *    
      *                                                            *    
      *------------------------------------------------------------*    
       ED-BETR-AUSPROV-ERMITT-ANF.                                      
241193     IF WK-ED-ST-NR-BETR = 8241950                                
241193     OR WK-ED-ST-NR-BETR = 8522200                                
241193     OR WK-ED-ST-NR-BETR = 8525030                                
190994     OR WK-ED-ST-NR-BETR = 8520300                                
190994     OR WK-ED-ST-NR-BETR = 8520110                                
241193        MOVE K-1                    TO Z-AUSPROV                  
241193     ELSE                                                         
070994        IF WK-ED-ST-NR-BETR = 8820600                             
070994          MOVE K-2                  TO Z-AUSPROV                  
070994        ELSE
110509          IF WK-ED-ST-NR-BETR = 8600010                           
110509             MOVE K-4               TO Z-AUSPROV
110509          ELSE                                                    
      *         UMSTELLEN DER DATUMSFORMATE AUF ISO                     
                MOVE Z-ZTPKT-DATE         TO Z-ZTPKT-DATE-EUR           
                PERFORM KONV-EUR-ISO                                    
                MOVE Z-ZTPKT-DATE-ISO     TO Z-VGL-DATE-1               
110509*         MOVE WB-MAT-ED-PROV-DAT1  IN WB-MAT-BETR                
110509          MOVE WB-ED-PROV-DAT1      IN WB-ED-BETR                 
                                           TO Z-ZTPKT-DATE-EUR          
                PERFORM KONV-EUR-ISO                                    
                MOVE Z-ZTPKT-DATE-ISO     TO Z-VGL-DATE-2               
                                                                        
110509*         EVALUATE WB-NI-MAT-ED-PROV-DAT1 IN WB-MAT-BETR NOT = - 1
110509*             ALSO WB-NI-MAT-ED-PROV-DAT2 IN WB-MAT-BETR NOT = - 1
110509          EVALUATE WB-NI-ED-PROV-DAT1 IN WB-ED-BETR NOT = - 1     
110509              ALSO WB-NI-ED-PROV-DAT2 IN WB-ED-BETR NOT = - 1     
                WHEN TRUE  ALSO FALSE                                   
      *       ES EX. NUR EINE ED-PROVISION                              
                  IF Z-VGL-DATE-1 >= Z-VGL-DATE-2                       
110509*              MOVE WB-MAT-ED-PROV1 IN WB-MAT-BETR   TO Z-AUSPROV 
110509               MOVE WB-ED-PROV1 IN WB-ED-BETR   TO Z-AUSPROV      
                  ELSE                                                  
      *              LOG. FEHLER                                        
      *              SIEHE ANNAHME 2!                                   
240893*              ANNAHME 2 ENTFÄLLT!                                
240893*              SET VERARB-LOG-FEHLER          TO TRUE             
240893*              MOVE 'ED-BETR-AUSPROV-ERMITT'  TO WK-PARAGRAPH     
240893*              MOVE  WK-FEHLER-TEXT6          TO WLR-FEHLER-TEXT  
240893               CONTINUE                                           
                  END-IF                                                
               WHEN TRUE  ALSO TRUE                                     
                  IF Z-VGL-DATE-1 >= Z-VGL-DATE-2                       
110509*               MOVE WB-MAT-ED-PROV1 IN WB-MAT-BETR   TO Z-AUSPROV
110509                MOVE WB-ED-PROV1 IN WB-ED-BETR   TO Z-AUSPROV     
                  ELSE                                                  
110509*               MOVE WB-MAT-ED-PROV2 IN WB-MAT-BETR   TO Z-AUSPROV
110509                MOVE WB-ED-PROV2 IN WB-ED-BETR   TO Z-AUSPROV     
                  END-IF                                                
               WHEN OTHER                                               
      *           LOG. FEHLER                                           
      *           SIEHE ANNAHME 1!                                      
                   SET VERARB-LOG-FEHLER          TO TRUE               
                   MOVE 'ED-BETR-AUSPROV-ERMITT'  TO WK-PARAGRAPH       
                   MOVE  WK-FEHLER-TEXT6          TO WLR-FEHLER-TEXT    
               END-EVALUATE
110509       END-IF                                                     
070994       END-IF                                                     
241193     END-IF                                                       
           .                                                            
       ED-BETR-AUSPROV-ERMITT-EXIT.                                     
           EXIT.                                                        
