      *****************************************************************         
CCC005*    EXEC SQL DECLARE G48_DEBITKARTE TABLE                                
CCC004*    ( G48_DKVERTRAGSNR               CHAR(15) NOT NULL,                  
CCC004*      G48_DKKONTOUNR                 CHAR(2) NOT NULL,                   
CCC004*      G48_DKWAEHRUNG                 CHAR(3) NOT NULL,                   
CCC004*      G48_DKKARTENNR                 CHAR(13) NOT NULL,                  
CCC004*      G48_DKKARTENART                CHAR(3) NOT NULL,                   
CCC004*      G48_DKGUELTIGBIS               CHAR(4) NOT NULL,                   
CCC004*      G48_DKDELKZ                    CHAR(1) NOT NULL,                   
CCC004*      G48_DKINHNAME1                 CHAR(27) NOT NULL,                  
CCC004*      G48_DKINHNAME2                 CHAR(27) NOT NULL,                  
CCC004*      G48_DKNFCFAEHIG                CHAR(1) NOT NULL,                   
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      G48_DKNFCSTATUS                CHAR(1) NOT NULL                    
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G48_DEBITKARTE              *               
      ******************************************************************        
       01  G48-DEBITKARTE-GESAMT.                                               
        05 NI-G48-DEBITKARTE-TAB.                                               
           10 NI-G48-DEBITKARTE-ELE PIC S9(04) COMP OCCURS 13 TIMES.            
        05 NI-G48-DEBITKARTE REDEFINES NI-G48-DEBITKARTE-TAB.                   
           10 NI-G48-DKVERTRAGSNR   PIC S9(4) COMP.                             
           10 NI-G48-DKKONTOUNR     PIC S9(4) COMP.                             
           10 NI-G48-DKWAEHRUNG     PIC S9(4) COMP.                             
           10 NI-G48-DKKARTENNR     PIC S9(4) COMP.                             
           10 NI-G48-DKKARTENART    PIC S9(4) COMP.                             
           10 NI-G48-DKGUELTIGBIS   PIC S9(4) COMP.                             
           10 NI-G48-DKDELKZ        PIC S9(4) COMP.                             
           10 NI-G48-DKINHNAME1     PIC S9(4) COMP.                             
           10 NI-G48-DKINHNAME2     PIC S9(4) COMP.                             
           10 NI-G48-DKNFCFAEHIG    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-G48-DKNFCSTATUS    PIC S9(4) COMP.                             
        05 DCL-G48-DEBITKARTE.                                                  
           10 G48-DKVERTRAGSNR     PIC X(15).                                   
           10 G48-DKKONTOUNR       PIC X(2).                                    
           10 G48-DKWAEHRUNG       PIC X(3).                                    
           10 G48-DKKARTENNR       PIC X(13).                                   
           10 G48-DKKARTENART      PIC X(3).                                    
           10 G48-DKGUELTIGBIS     PIC X(4).                                    
           10 G48-DKDELKZ          PIC X(1).                                    
           10 G48-DKINHNAME1       PIC X(27).                                   
           10 G48-DKINHNAME2       PIC X(27).                                   
           10 G48-DKNFCFAEHIG      PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LETZTERAENDERER      PIC X(10).                                   
           10 G48-DKNFCSTATUS      PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
