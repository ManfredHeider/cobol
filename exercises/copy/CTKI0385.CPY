CCC005*    EXEC SQL DECLARE LBNPSZAHLUNG TABLE                                  
CCC004*    ( LPZVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LPZLFDNR                       SMALLINT NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      LPZGEVO                        CHAR(20) NOT NULL,                  
CCC004*      LPZDATUM                       DATE NOT NULL,                      
CCC004*      LPZBETRAG                      DECIMAL(13, 2) NOT NULL             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE LBNPSZAHLUNG                *               
      ******************************************************************        
       01  LBNPSZAHLUNG-GESAMT.                                                 
        05 NI-LBNPSZAHLUNG-TAB.                                                 
           10 NI-LBNPSZAHLUNG-ELE PIC S9(04) COMP OCCURS 7 TIMES.               
        05 NI-LBNPSZAHLUNG REDEFINES NI-LBNPSZAHLUNG-TAB.                       
           10 NI-LPZVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LPZLFDNR           PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-LPZGEVO            PIC S9(4) COMP.                             
           10 NI-LPZDATUM           PIC S9(4) COMP.                             
           10 NI-LPZBETRAG          PIC S9(4) COMP.                             
        05 DCL-LBNPSZAHLUNG.                                                    
           10 LPZVERTRAGSID        PIC X(13).                                   
           10 LPZLFDNR             PIC S9(4) USAGE COMP.                        
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LETZTERAENDERER      PIC X(10).                                   
           10 LPZGEVO              PIC X(20).                                   
           10 LPZDATUM             PIC X(10).                                   
           10 LPZBETRAG            PIC S9(11)V9(2) USAGE COMP-3.                
      ************************** ENDE-DCLGEN *************************          
