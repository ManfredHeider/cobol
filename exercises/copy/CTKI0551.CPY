      *****************************************************************         
CCC005*    EXEC SQL DECLARE BANKDEBITKARTE TABLE                                
CCC004*    ( BDKVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      BDKLFDNR                       SMALLINT NOT NULL,                  
CCC004*      BDKWAEHRUNG                    SMALLINT NOT NULL,                  
CCC004*      BDKKARTENNR                    CHAR(13) NOT NULL,                  
CCC004*      BDKKARTENART                   CHAR(3),                            
CCC004*      BDKGUELTIGBIS                  CHAR(4),                            
CCC004*      BDKINHABERNAME1                CHAR(30),                           
CCC004*      BDKINHABERNAME2                CHAR(30),                           
CCC004*      BDKNFCKZN                      CHAR(1),                            
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      BDKNFCSTATUS                   CHAR(1)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE BANKDEBITKARTE              *               
      ******************************************************************        
       01  BANKDEBITKARTE-GESAMT.                                               
        05 NI-BANKDEBITKARTE-TAB.                                               
           10 NI-BANKDEBITKARTE-ELE PIC S9(04) COMP OCCURS 12 TIMES.            
        05 NI-BANKDEBITKARTE REDEFINES NI-BANKDEBITKARTE-TAB.                   
           10 NI-BDKVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-BDKLFDNR           PIC S9(4) COMP.                             
           10 NI-BDKWAEHRUNG        PIC S9(4) COMP.                             
           10 NI-BDKKARTENNR        PIC S9(4) COMP.                             
           10 NI-BDKKARTENART       PIC S9(4) COMP.                             
           10 NI-BDKGUELTIGBIS      PIC S9(4) COMP.                             
           10 NI-BDKINHABERNAME1    PIC S9(4) COMP.                             
           10 NI-BDKINHABERNAME2    PIC S9(4) COMP.                             
           10 NI-BDKNFCKZN          PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-BDKNFCSTATUS       PIC S9(4) COMP.                             
        05 DCL-BANKDEBITKARTE.                                                  
           10 BDKVERTRAGSID        PIC X(13).                                   
           10 BDKLFDNR             PIC S9(4) USAGE COMP-5.                      
           10 BDKWAEHRUNG          PIC S9(4) USAGE COMP-5.                      
           10 BDKKARTENNR          PIC X(13).                                   
           10 BDKKARTENART         PIC X(3).                                    
           10 BDKGUELTIGBIS        PIC X(4).                                    
           10 BDKINHABERNAME1      PIC X(30).                                   
           10 BDKINHABERNAME2      PIC X(30).                                   
           10 BDKNFCKZN            PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LETZTERAENDERER      PIC X(10).                                   
           10 BDKNFCSTATUS         PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
