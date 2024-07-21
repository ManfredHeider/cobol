      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21/09/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0445.CPY  $                                         


      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTKI0445.CPv  $                            
      *
      *   Rev 2.2   Oct 25 2010 12:03:52   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *
      *   Rev 2.1   Sep 29 2010 10:51:30   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 21 2010 14:42:46   T13EX14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
CCC005*    EXEC SQL DECLARE SACHWIEN TABLE                              
CCC004*    ( SWIVERTRAGSID                  CHAR(13) NOT NULL,          
CCC004*      SWIVEZLFDNR                    SMALLINT NOT NULL,          
CCC004*      SWIHOSTID                      CHAR(15) NOT NULL,          
CCC004*      SWIUNFSUMME                    DECIMAL(13, 2),
CCC004*      SWIRENTENART_KTY               SMALLINT,                   
CCC004*      SWIRENTENART_KNR               DECIMAL(15, 0),             
CCC004*      SWIUNFKAPINVPROZ               DECIMAL(3, 0),              
CCC004*      SWIINVSUMME                    DECIMAL(13, 2),             
CCC004*      SWIUNFALLTODPR                 DECIMAL(13, 2),             
CCC004*      SWIVERSSUMME                   DECIMAL(13, 2),             
CCC004*      SWITAGEGELD                    DECIMAL(13, 2),             
CCC004*      SWITAGEGELDTT                  SMALLINT,                   
CCC004*      SWIUNFKAPVAR_KTY               SMALLINT,                   
CCC004*      SWIUNFKAPVAR_KNR               DECIMAL(15, 0),             
CCC004*      SWIKFZKZ                       CHAR(12),                   
CCC004*      SWIGESAMTBEITRAG               DECIMAL(13, 2),             
CCC004*      SWIRATENBEITRAG                DECIMAL(13, 2),             
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                  
CCC004*      SWIBUENDLBEZ_KTY               SMALLINT,                   
CCC004*      SWIBUENDLBEZ_KNR               DECIMAL(15, 0)              
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SACHWIEN                    *       
      ******************************************************************
       01  SACHWIEN-GESAMT.                                             
        05 NI-SACHWIEN-TAB.                                             
           10 NI-SACHWIEN-ELE PIC S9(04) COMP OCCURS 20 TIMES.          
        05 NI-SACHWIEN REDEFINES NI-SACHWIEN-TAB.                       
           10 NI-SWIVERTRAGSID      PIC S9(4) COMP.                     
           10 NI-SWIVEZLFDNR        PIC S9(4) COMP.                     
           10 NI-SWIHOSTID          PIC S9(4) COMP.                     
           10 NI-SWIUNFSUMME        PIC S9(4) COMP.                     
           10 NI-SWIRENTENART-KTY   PIC S9(4) COMP.                     
           10 NI-SWIRENTENART-KNR   PIC S9(4) COMP.                     
           10 NI-SWIUNFKAPINVPROZ   PIC S9(4) COMP.                     
           10 NI-SWIINVSUMME        PIC S9(4) COMP.                     
           10 NI-SWIUNFALLTODPR     PIC S9(4) COMP.                     
           10 NI-SWIVERSSUMME       PIC S9(4) COMP.                     
           10 NI-SWITAGEGELD        PIC S9(4) COMP.                     
           10 NI-SWITAGEGELDTT      PIC S9(4) COMP.                     
           10 NI-SWIUNFKAPVAR-KTY   PIC S9(4) COMP.                     
           10 NI-SWIUNFKAPVAR-KNR   PIC S9(4) COMP.                     
           10 NI-SWIKFZKZ           PIC S9(4) COMP.                     
           10 NI-SWIGESAMTBEITRAG   PIC S9(4) COMP.                     
           10 NI-SWIRATENBEITRAG    PIC S9(4) COMP.                     
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                     
           10 NI-SWIBUENDLBEZ-KTY   PIC S9(4) COMP.                     
           10 NI-SWIBUENDLBEZ-KNR   PIC S9(4) COMP.                     
        05 DCL-SACHWIEN.                                                
           10 SWIVERTRAGSID        PIC X(13).                           
           10 SWIVEZLFDNR          PIC S9(4) USAGE COMP.                
           10 SWIHOSTID            PIC X(15).                           
           10 SWIUNFSUMME          PIC S9(11)V9(2) USAGE COMP-3.
           10 SWIRENTENART-KTY     PIC S9(4) USAGE COMP.                
           10 SWIRENTENART-KNR     PIC S9(15)V USAGE COMP-3.            
           10 SWIUNFKAPINVPROZ     PIC S9(3)V USAGE COMP-3.             
           10 SWIINVSUMME          PIC S9(11)V9(2) USAGE COMP-3.        
           10 SWIUNFALLTODPR       PIC S9(11)V9(2) USAGE COMP-3.        
           10 SWIVERSSUMME         PIC S9(11)V9(2) USAGE COMP-3.        
           10 SWITAGEGELD          PIC S9(11)V9(2) USAGE COMP-3.        
           10 SWITAGEGELDTT        PIC S9(4) USAGE COMP.                
           10 SWIUNFKAPVAR-KTY     PIC S9(4) USAGE COMP.                
           10 SWIUNFKAPVAR-KNR     PIC S9(15)V USAGE COMP-3.            
           10 SWIKFZKZ             PIC X(12).                           
           10 SWIGESAMTBEITRAG     PIC S9(11)V9(2) USAGE COMP-3.        
           10 SWIRATENBEITRAG      PIC S9(11)V9(2) USAGE COMP-3.        
           10 LETZTEAENDERUNG      PIC X(26).                           
           10 SWIBUENDLBEZ-KTY     PIC S9(4) USAGE COMP.                
           10 SWIBUENDLBEZ-KNR     PIC S9(15)V USAGE COMP-3.            
      ************************** ENDE-DCLGEN *************************  
