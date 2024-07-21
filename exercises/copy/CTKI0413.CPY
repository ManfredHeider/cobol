      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  19.05.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0413.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTKI0413.CPv  $                            
      *
      *   Rev 2.2   Nov 21 2013 10:12:54   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   May 29 2006 10:22:08   t13ex27                      
      *AKI00594: DB-Žnderung f?r Fix 88 DCLGENs                         
      *                                                                 
      *   Rev 2.0   May 19 2006 08:41:58   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ******************************************************************
      * DCLGEN TABLE KFZBESVB                                           
      ******************************************************************
CCC005*    EXEC SQL DECLARE KFZBESVB TABLE                              
CCC004*    ( KFZBVBVERTRAGSID               CHAR(13) NOT NULL,          
CCC004*      KFZBVBLFDNR                    SMALLINT NOT NULL,          
CCC004*      KFZBVBBESVEREINB               CHAR(50)                    
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KFZBESVB                    *       
      ******************************************************************
       01  KFZBESVB-GESAMT.                                             
        05 NI-KFZBESVB-TAB.                                             
           10 NI-KFZBESVB-ELE PIC S9(04) COMP OCCURS 3 TIMES.           
        05 NI-KFZBESVB REDEFINES NI-KFZBESVB-TAB.                       
           10 NI-KFZBVBVERTRAGSID   PIC S9(4) COMP.                     
           10 NI-KFZBVBLFDNR        PIC S9(4) COMP.                     
           10 NI-KFZBVBBESVEREINB   PIC S9(4) COMP.                     
        05 DCL-KFZBESVB.                                                
           10 KFZBVBVERTRAGSID     PIC X(13).                           
           10 KFZBVBLFDNR          PIC S9(4) USAGE COMP.                
           10 KFZBVBBESVEREINB     PIC X(50).                           
      ************************** ENDE-DCLGEN *************************  
