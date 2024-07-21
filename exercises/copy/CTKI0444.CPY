      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  10/08/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0444.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTKI0444.CPv  $                                
      *
      *   Rev 2.2   Sep 29 2010 10:51:24   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Aug 10 2010 10:52:42   T13EX14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
CCC005*    EXEC SQL DECLARE VERTRAGURLBEZ TABLE                         
CCC004*    ( VUBVERTRAGSID                  CHAR(13) NOT NULL,          
CCC004*      VUBURL                         VARCHAR(100) NOT NULL,      
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,         
CCC004*      VUBRDTAZIEL                    SMALLINT                    
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERTRAGURLBEZ               *       
      ******************************************************************
       01  VERTRAGURLBEZ-GESAMT.                                        
        05 NI-VERTRAGURLBEZ-TAB.                                        
           10 NI-VERTRAGURLBEZ-ELE PIC S9(04) COMP OCCURS 4 TIMES.      
        05 NI-VERTRAGURLBEZ REDEFINES NI-VERTRAGURLBEZ-TAB.             
           10 NI-VUBVERTRAGSID      PIC S9(4) COMP.                     
           10 NI-VUBURL             PIC S9(4) COMP.                     
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                     
           10 NI-VUBRDTAZIEL        PIC S9(4) COMP.                     
        05 DCL-VERTRAGURLBEZ.                                           
           10 VUBVERTRAGSID        PIC X(13).                           
           10 VUBURL.                                                   
              49 VUBURL-LEN        PIC S9(4) USAGE COMP.                
              49 VUBURL-TEXT       PIC X(100).                          
           10 LETZTEAENDERUNG      PIC X(26).                           
           10 VUBRDTAZIEL          PIC S9(4) USAGE COMP.                
      ************************** ENDE-DCLGEN *************************  
