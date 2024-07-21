      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0007.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 11:17:32   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Rest
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:21:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DPVB0007                                      * 
      *    LESEN DER ED-PROVISION DES ED-BESITZER ZUM BETREUENDEN     * 
      *    ED-KONTO AUS DER MAT, DIE AKTUELL                          * 
      *    A) UNMITTELBAR VOR DEM ZEITPUNKT BZW.                      * 
      *    B) ZU DEM ZEITPUNKT                                        * 
      *    GALT/GILT.                                                 * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 
      *    DATUM:    01.04.1993                                       * 
      ***************************************************************** 
      *    VERSION   01   16.09.93                                   *  
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !31.08.93!    ! SELECT-ANPASSUNG AN DEN BESCHLUß DER *  
      *         !        !    ! PROJEKTSITZUNG VOM 07.05.1993        *  
      *         !16.09.93! 01 ! Prod.übergabe             ! NAUMANN  *  
      * AVB01029!11.05.09!2.1 ! ED-Prov in Tabelle ED an- ! Walb     *  
      *         !        !    ! statt MAT                 !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *---------------------------------------------------------------  
       ED-BES-ED-PROV-LESEN SECTION.                                    
      *---------------------------------------------------------------  
      *-INPUT:  WK-ED-ST-NR-BETR                                     -  
310893*-        Z-ZTPKT-TSTMP NICHT MEHR!                            -  
310893*-        Z-ZTPKT-DATE                                         -  
      *---------------------------------------------------------------  
      *-OUTPUT: WB-MAT-BETR                                          -  
      *---------------------------------------------------------------  
       ED-BES-ED-PROV-LESEN-ANF.                                        
110509*    INITIALIZE WB-MAT-BETR                                       
110509     INITIALIZE WB-ED-BETR                                        
                                                                        
           PERFORM  WITH TEST AFTER                                     
                    VARYING  IND-I  FROM   1  BY  1                     
                                    UNTIL  IND-I   >  WK-LESEN-MAX      
                                     OR    (SQLCODE  NOT =  -904)       
CCC005*    EXEC SQL                                                     
CCC004*       SELECT                                                    
110509*             MAT1.MA_ST_NR,                                      
110509*             MAT_DAT_VON,                                        
110509*             MAT_LAST_UPDATE,                                    
110509*             MAT_DAT_BIS,                                        
110509*             MAT_KUE_DAT_VOM,                                    
110509*             MAT_EING_KUE_DAT,                                   
110509*             MAT_VORZFREI_DAT,                                   
110509*             KTY_MAT_AUS_GRND,                                   
110509*             KNR_MAT_AUS_GRND,                                   
110509*             MAT_L_RCK_STZ,                                      
110509*             MAT_ED_PROV1,                                       
110509*             MAT_ED_PROV2,                                       
110509*             MAT_ED_PROV_DAT1,                                   
110509*             MAT_ED_PROV_DAT2                                    
CCC004*             ED_ST_NR,
CCC004*             ED_LAST_UPDATE,
CCC004*             MA_ST_NR,
CCC004*             ED_RCK_STZ,
CCC004*             ED_RCK_STZ_DAT,
CCC004*             ED_BEZ,
CCC004*             ED_GENERATION,
CCC004*             ED_ZWEIG,
CCC004*             ED_ZWEIG_VOM,
CCC004*             ED_PROV1,
CCC004*             ED_PROV_DAT1,
CCC004*             ED_PROV2,
CCC004*             ED_PROV_DAT2
CCC004*       INTO                                                      
110509*             :DCL-VIVB0049.MA-ST-NR                              
110509*              :NI-VIVB0049.NI-MA-ST-NR                           
110509*            ,:DCL-VIVB0049.MAT-DAT-VON                           
110509*              :NI-VIVB0049.NI-MAT-DAT-VON                        
110509*            ,:DCL-VIVB0049.MAT-LAST-UPDATE                       
110509*              :NI-VIVB0049.NI-MAT-LAST-UPDATE                    
110509*            ,:DCL-VIVB0049.MAT-DAT-BIS                           
110509*              :NI-VIVB0049.NI-MAT-DAT-BIS                        
110509*            ,:DCL-VIVB0049.MAT-KUE-DAT-VOM                       
110509*              :NI-VIVB0049.NI-MAT-KUE-DAT-VOM                    
110509*            ,:DCL-VIVB0049.MAT-EING-KUE-DAT                      
110509*              :NI-VIVB0049.NI-MAT-EING-KUE-DAT                   
110509*            ,:DCL-VIVB0049.MAT-VORZFREI-DAT                      
110509*              :NI-VIVB0049.NI-MAT-VORZFREI-DAT                   
110509*            ,:DCL-VIVB0049.KTY-MAT-AUS-GRND                      
110509*              :NI-VIVB0049.NI-KTY-MAT-AUS-GRND                   
110509*            ,:DCL-VIVB0049.KNR-MAT-AUS-GRND                      
110509*              :NI-VIVB0049.NI-KNR-MAT-AUS-GRND                   
110509*            ,:DCL-VIVB0049.MAT-L-RCK-STZ                         
110509*              :NI-VIVB0049.NI-MAT-L-RCK-STZ                      
110509*            ,:DCL-VIVB0049.MAT-ED-PROV1                          
110509*              :NI-VIVB0049.NI-MAT-ED-PROV1                       
110509*            ,:DCL-VIVB0049.MAT-ED-PROV2                          
110509*              :NI-VIVB0049.NI-MAT-ED-PROV2                       
110509*            ,:DCL-VIVB0049.MAT-ED-PROV-DAT1                      
110509*              :NI-VIVB0049.NI-MAT-ED-PROV-DAT1                   
110509*            ,:DCL-VIVB0049.MAT-ED-PROV-DAT2                      
110509*              :NI-VIVB0049.NI-MAT-ED-PROV-DAT2                   
CCC004*              :DCL-VIVBA003.ED-ST-NR
CCC004*               :NI-VIVBA003.NI-ED-ST-NR,
CCC004*              :DCL-VIVBA003.ED-LAST-UPDATE
CCC004*               :NI-VIVBA003.NI-ED-LAST-UPDATE,
CCC004*              :DCL-VIVBA003.MA-ST-NR
CCC004*               :NI-VIVBA003.NI-MA-ST-NR,
CCC004*              :DCL-VIVBA003.ED-RCK-STZ
CCC004*               :NI-VIVBA003.NI-ED-RCK-STZ,
CCC004*              :DCL-VIVBA003.ED-RCK-STZ-DAT
CCC004*               :NI-VIVBA003.NI-ED-RCK-STZ-DAT,
CCC004*              :DCL-VIVBA003.ED-BEZ
CCC004*               :NI-VIVBA003.NI-ED-BEZ,
CCC004*              :DCL-VIVBA003.ED-GENERATION
CCC004*               :NI-VIVBA003.NI-ED-GENERATION,
CCC004*              :DCL-VIVBA003.ED-ZWEIG
CCC004*               :NI-VIVBA003.NI-ED-ZWEIG,
CCC004*              :DCL-VIVBA003.ED-ZWEIG-VOM
CCC004*               :NI-VIVBA003.NI-ED-ZWEIG-VOM,
CCC004*              :DCL-VIVBA003.ED-PROV1
CCC004*               :NI-VIVBA003.NI-ED-PROV1,
CCC004*              :DCL-VIVBA003.ED-PROV-DAT1
CCC004*               :NI-VIVBA003.NI-ED-PROV-DAT1,
CCC004*              :DCL-VIVBA003.ED-PROV2
CCC004*               :NI-VIVBA003.NI-ED-PROV2,
CCC004*              :DCL-VIVBA003.ED-PROV-DAT2
CCC004*               :NI-VIVBA003.NI-ED-PROV-DAT2
CCC004*       FROM                                                      
110509*             VIVB0049 MAT1, VIVB0003 ED                          
CCC004*             VIVBA003 ED                                         
CCC004*       WHERE                                                     
CCC004*             ED.ED_ST_NR     =   :WK-ED-ST-NR-BETR               
110509*        AND  ED.MA_ST_NR     =    MAT1.MA_ST_NR                  
110509*310893  AND  MAT_LAST_UPDATE =                                   
110509*310893      (SELECT MAX(MAT2.MAT_LAST_UPDATE)                    
110509*310893        FROM                                               
110509*310893           VIVB0049 MAT2                                   
110509*310893        WHERE  MAT1.MA_ST_NR = MAT2.MA_ST_NR               
110509*310893          AND  DATE(MAT2.MAT_DAT_VON) <= :Z-ZTPKT-DATE     
110509*310893       )                                                   
310893*        AND  MAT1.MAT_DAT_VON     =                              
310893*             (SELECT MAX(MAT2.MAT_DAT_VON)                       
310893*              FROM                                               
310893*                 VIVB0049 MAT2                                   
310893*              WHERE MAT1.MA_ST_NR       = MAT2.MA_ST_NR          
310893*                AND MAT2.MAT_DAT_VON   <= :Z-ZTPKT-TSTMP         
310893*             )                                                   
CCC003*    END-EXEC                                                     
                                                                        
           END-PERFORM                                                  
                                                                        
080993D    DISPLAY 'WK-ED-ST-NR-BETR = ' WK-ED-ST-NR-BETR               
080993D    DISPLAY 'Z-ZTPKT-DATE     = ' Z-ZTPKT-DATE                   
                                                                        
           EVALUATE   SQLCODE                                           
           WHEN       0                                                 
110509*          MOVE VIVB0049-GESAMT         TO WB-MAT-BETR            
110509           MOVE VIVBA003-GESAMT         TO WB-ED-BETR             
           WHEN       100                                               
                 SET VERARB-LOG-FEHLER        TO TRUE                   
                 MOVE 'ED-BES-ED-PROV-LESEN'  TO WK-PARAGRAPH           
                 MOVE  WK-FEHLER-TEXT6        TO WLR-FEHLER-TEXT        
           WHEN       OTHER                                             
                 SET VERARB-SQL-CODE-EX    TO TRUE                      
                 MOVE SQLCODE              TO WLR-SQL-CODE              
                 IF    SQLERRML       >   ZERO                          
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         
                  ELSE                                                  
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         
                 END-IF                                                 
           END-EVALUATE                                                 
           .                                                            
       ED-BES-ED-PROV-LESEN-EXIT.                                       
           EXIT.                                                        
