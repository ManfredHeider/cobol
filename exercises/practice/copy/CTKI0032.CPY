      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  20/09/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0032.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTKI0032.CPv  $                                
      *
      *   Rev 2.1   Sep 29 2010 10:51:18   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 20 2010 13:06:44   T13EX27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
CCC005*    EXEC SQL DECLARE LAND TABLE                                  
CCC004*    ( LANDINTERNATKZ                 CHAR(3) NOT NULL,           
CCC004*      LANDTEXT                       CHAR(30) NOT NULL,          
CCC004*      LANDISOKZ                      CHAR(2) NOT NULL            
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LAND                        *       
      ******************************************************************
       01  LAND-GESAMT.                                                 
        05 NI-LAND-TAB.                                                 
           10 NI-LAND-ELE PIC S9(04) COMP OCCURS 3 TIMES.               
        05 NI-LAND REDEFINES NI-LAND-TAB.                               
           10 NI-LANDINTERNATKZ     PIC S9(4) COMP.                     
           10 NI-LANDTEXT           PIC S9(4) COMP.                     
           10 NI-LANDISOKZ          PIC S9(4) COMP.                     
        05 DCL-LAND.                                                    
           10 LANDINTERNATKZ       PIC X(3).                            
           10 LANDTEXT             PIC X(30).                           
           10 LANDISOKZ            PIC X(2).                            
      ************************** ENDE-DCLGEN *************************  
