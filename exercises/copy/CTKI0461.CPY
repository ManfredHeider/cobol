      *****************************************************************         
CCC005*    EXEC SQL DECLARE VERTRAGZAHLUNG TABLE                                
CCC004*    ( VZAVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      VZALFDNR                       SMALLINT NOT NULL,                  
CCC004*      VZADAUER                       SMALLINT NOT NULL,                  
CCC004*      VZABEGINN                      DATE,                               
CCC004*      VZAZAHLBETRAG                  DECIMAL(13, 2),                     
CCC004*      VZAERSTELLTDAT                 DATE NOT NULL,                      
CCC004*      VZALFDMONRECHPGM               SMALLINT,                           
CCC004*      VZAANZMONVERARB                SMALLINT,                           
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      VZATSCHICHTNR                  SMALLINT,                           
CCC004*      VZAKLBERHAUSSCHL               CHAR(1) NOT NULL                    
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VERTRAGZAHLUNG              *               
      ******************************************************************        
       01  VERTRAGZAHLUNG-GESAMT.                                               
        05 NI-VERTRAGZAHLUNG-TAB.                                               
           10 NI-VERTRAGZAHLUNG-ELE PIC S9(04) COMP OCCURS 11 TIMES.            
        05 NI-VERTRAGZAHLUNG REDEFINES NI-VERTRAGZAHLUNG-TAB.                   
           10 NI-VZAVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-VZALFDNR           PIC S9(4) COMP.                             
           10 NI-VZADAUER           PIC S9(4) COMP.                             
           10 NI-VZABEGINN          PIC S9(4) COMP.                             
           10 NI-VZAZAHLBETRAG      PIC S9(4) COMP.                             
           10 NI-VZAERSTELLTDAT     PIC S9(4) COMP.                             
           10 NI-VZALFDMONRECHPGM   PIC S9(4) COMP.                             
           10 NI-VZAANZMONVERARB    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-VZATSCHICHTNR      PIC S9(4) COMP.                             
           10 NI-VZAKLBERHAUSSCHL   PIC S9(4) COMP.                             
        05 DCL-VERTRAGZAHLUNG.                                                  
           10 VZAVERTRAGSID        PIC X(13).                                   
           10 VZALFDNR             PIC S9(4) USAGE COMP.                        
           10 VZADAUER             PIC S9(4) USAGE COMP.                        
           10 VZABEGINN            PIC X(10).                                   
           10 VZAZAHLBETRAG        PIC S9(11)V9(2) USAGE COMP-3.                
           10 VZAERSTELLTDAT       PIC X(10).                                   
           10 VZALFDMONRECHPGM     PIC S9(4) USAGE COMP.                        
           10 VZAANZMONVERARB      PIC S9(4) USAGE COMP.                        
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 VZATSCHICHTNR        PIC S9(4) USAGE COMP.                        
           10 VZAKLBERHAUSSCHL     PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
