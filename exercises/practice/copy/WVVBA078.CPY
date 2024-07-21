      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  Januar 2001                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   WVVBA078.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/WVVBA078.CPv  $                                    
      *
      *   Rev 2.1   23 Oct 2001 15:59:00   t13ae27
      *neu
      *                                                                         
      *   Rev 2.0   03 Sep 2001 10:55:14   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVBA078                           *        
      ******************************************************************        
        05  WB-NI-VIVBA078-TAB.                                                 
           10 WB-NI-VIVBA078-ELE   PIC S9(04)  BINARY OCCURS 11 TIMES.          
        05  WB-NI-VIVBA078         REDEFINES   WB-NI-VIVBA078-TAB.              
           10 WB-NI-MA-ST-NR               PIC S9(04)       BINARY.             
           10 WB-NI-KTY-WB-TYP             PIC S9(04)       BINARY.             
           10 WB-NI-KNR-WB-TYP             PIC S9(04)       BINARY.             
           10 WB-NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.             
           10 WB-NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.             
           10 WB-NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.             
           10 WB-NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.             
           10 WB-NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.             
           10 WB-NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.             
           10 WB-NI-KAND-NEU-AB            PIC S9(04)       BINARY.             
           10 WB-NI-KAND-RUECKERM          PIC S9(04)       BINARY.             
                                                                                
        05  WB-VIVBA078.                                                        
           10 WB-MA-ST-NR                 PIC S9(009)       BINARY.             
           10 WB-KTY-WB-TYP               PIC S9(004)       BINARY.             
           10 WB-KNR-WB-TYP               PIC S9(004)       BINARY.             
           10 WB-KTY-WERTUNGS-TYP         PIC S9(004)       BINARY.             
           10 WB-KNR-WERTUNGS-TYP         PIC S9(004)       BINARY.             
           10 WB-KAND-WERTUNGSSTUFE       PIC S9(004)       BINARY.             
           10 WB-KAND-LAST-UPDATE         PIC X(026).                           
           10 WB-KAND-TAETIG-BIS          PIC X(010).                           
           10 WB-KAND-DRUCK-KZ            PIC S9(004)       BINARY.             
           10 WB-KAND-NEU-AB              PIC X(010).                           
           10 WB-KAND-RUECKERM            PIC X(001).                           
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *        
      ******************************************************************        
