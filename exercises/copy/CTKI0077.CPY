      *****************************************************************         
CCC005*    EXEC SQL DECLARE KUNDENUEBERTRAGUNG TABLE                            
CCC004*    ( VERTRAGSID                     CHAR(13) NOT NULL,                  
CCC004*      KDULAUFENDENUMMER              SMALLINT NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      VBALTID                        CHAR(13) NOT NULL,                  
CCC004*      VBALTROLLENKZ                  CHAR(1) NOT NULL,                   
CCC004*      VBNEUID                        CHAR(13) NOT NULL,                  
CCC004*      VBNEUROLLENKZ                  CHAR(1) NOT NULL,                   
CCC004*      DTAERLEDIGTKZ                  CHAR(1) NOT NULL,                   
CCC004*      KDUEBERTRAGUNGSART             CHAR(1) NOT NULL,                   
CCC004*      KDUGUELTIGABDAT                DATE NOT NULL,                      
CCC004*      KDUBEARBEITER                  CHAR(30) NOT NULL,                  
CCC004*      KDUEIGENVERMKZ                 CHAR(1) NOT NULL,                   
CCC004*      KDUKKVLID                      CHAR(13),                           
CCC004*      KDUTICKETID                    DECIMAL(18, 0),                     
CCC004*      KDUWIDERSPRUCH                 CHAR(1),                            
CCC004*      KDUWARTEZEITKZ                 CHAR(1)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE KUNDENUEBERTRAGUNG          *               
      ******************************************************************        
       01  KUNDENUEBERTRAGUNG-GESAMT.                                           
        05 NI-KUNDENUEBERTRAGUNG-TAB.                                           
           10 NI-KUNDENUEBERTRAGUNG-ELE PIC S9(04) COMP OCCURS 16 TIMES.        
        05 NI-KUNDENUEBERTRAGUNG REDEFINES NI-KUNDENUEBERTRAGUNG-TAB.           
           10 NI-VERTRAGSID         PIC S9(4) COMP.                             
           10 NI-KDULAUFENDENUMMER  PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-VBALTID            PIC S9(4) COMP.                             
           10 NI-VBALTROLLENKZ      PIC S9(4) COMP.                             
           10 NI-VBNEUID            PIC S9(4) COMP.                             
           10 NI-VBNEUROLLENKZ      PIC S9(4) COMP.                             
           10 NI-DTAERLEDIGTKZ      PIC S9(4) COMP.                             
           10 NI-KDUEBERTRAGUNGSART PIC S9(4) COMP.                             
           10 NI-KDUGUELTIGABDAT    PIC S9(4) COMP.                             
           10 NI-KDUBEARBEITER      PIC S9(4) COMP.                             
           10 NI-KDUEIGENVERMKZ     PIC S9(4) COMP.                             
           10 NI-KDUKKVLID          PIC S9(4) COMP.                             
           10 NI-KDUTICKETID        PIC S9(4) COMP.                             
           10 NI-KDUWIDERSPRUCH     PIC S9(4) COMP.                             
           10 NI-KDUWARTEZEITKZ     PIC S9(4) COMP.                             
        05 DCL-KUNDENUEBERTRAGUNG.                                              
           10 VERTRAGSID           PIC X(13).                                   
           10 KDULAUFENDENUMMER    PIC S9(4) USAGE COMP.                        
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 VBALTID              PIC X(13).                                   
           10 VBALTROLLENKZ        PIC X(1).                                    
           10 VBNEUID              PIC X(13).                                   
           10 VBNEUROLLENKZ        PIC X(1).                                    
           10 DTAERLEDIGTKZ        PIC X(1).                                    
           10 KDUEBERTRAGUNGSART   PIC X(1).                                    
           10 KDUGUELTIGABDAT      PIC X(10).                                   
           10 KDUBEARBEITER        PIC X(30).                                   
           10 KDUEIGENVERMKZ       PIC X(1).                                    
           10 KDUKKVLID            PIC X(13).                                   
           10 KDUTICKETID          PIC S9(18)V USAGE COMP-3.                    
           10 KDUWIDERSPRUCH       PIC X(1).                                    
           10 KDUWARTEZEITKZ       PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
