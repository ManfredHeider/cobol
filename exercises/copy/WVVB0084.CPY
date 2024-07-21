      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   WVVB0084.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/WVVB0084.CPv  $                                    
      *
      *   Rev 2.1   Jul 10 2002 14:33:12   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *                                                                         
      *   Rev 2.0   20 Feb 2001 13:29:36   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0084                           *        
      ******************************************************************        
        05  WB-NI-VIVB0084-TAB.                                                 
           10 WB-NI-VIVB0084-ELE    PIC S9(04) BINARY OCCURS  7  TIMES.
        05  WB-NI-VIVB0084            REDEFINES   WB-NI-VIVB0084-TAB.           
           10 WB-NI-MA-ST-NR                PIC S9(04)  BINARY.                 
           10 WB-NI-MURLZ-DAT-VON           PIC S9(04)  BINARY.                 
           10 WB-NI-MURLZ-SST-KZ            PIC S9(04)  BINARY.
           10 WB-NI-MURLZ-LAST-UPDATE       PIC S9(04)  BINARY.                 
           10 WB-NI-MURLZ-SST-DAT           PIC S9(04)  BINARY.
           10 WB-NI-MURLZ-DAT-BIS           PIC S9(04)  BINARY.                 
           10 WB-NI-MURLZ-URL               PIC S9(04)  BINARY.
        05 WB-VIVB0084.                                                         
           10 WB-MA-ST-NR                   PIC S9(9)   USAGE COMP.             
           10 WB-MURLZ-DAT-VON              PIC X(26).                          
           10 WB-MURLZ-SST-KZ               PIC X(01).
           10 WB-MURLZ-LAST-UPDATE          PIC X(26).
           10 WB-MURLZ-SST-DAT              PIC X(10).
           10 WB-MURLZ-DAT-BIS              PIC X(10).                          
           10 WB-MURLZ-URL                  PIC X(70).
                                                                                
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************        
