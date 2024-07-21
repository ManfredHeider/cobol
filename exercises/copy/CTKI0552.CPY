      *****************************************************************         
CCC005*    EXEC SQL DECLARE G40_GELDKONTO TABLE                                 
CCC004*    ( G40GKOVNR                      CHAR(15) NOT NULL,                  
CCC004*      G40GKODATOEFF                  CHAR(8) NOT NULL,                   
CCC004*      G40GKOSCHLDAT                  CHAR(8) NOT NULL,                   
CCC004*      G40GKOBILANZSCHL               CHAR(4) NOT NULL,                   
CCC004*      G40GKOEXTLIMIT                 DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKVORZEICHEN                CHAR(1) NOT NULL,                   
CCC004*      G40GKOSALDO                    DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKOKONDSCHL                 CHAR(2) NOT NULL,                   
CCC004*      G40GKODELKZ                    CHAR(1) NOT NULL,                   
CCC004*      G40GKOKUMHABENAM               DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKOKUMSOLLAM                DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKOKUMHABENVM               DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKOKUMSOLLVM                DECIMAL(14, 2) NOT NULL,            
CCC004*      G40GKOZRAM                     CHAR(5) NOT NULL,                   
CCC004*      G40GKOZRVM                     CHAR(5) NOT NULL,                   
CCC004*      G40GKOUEBERZINS                DECIMAL(6, 4) NOT NULL,             
CCC004*      G40GKOHABENZINS                DECIMAL(6, 4) NOT NULL,             
CCC004*      G40GKOSOLLZINS                 DECIMAL(6, 4) NOT NULL,             
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G40_GELDKONTO               *               
      ******************************************************************        
       01  G40-GELDKONTO-GESAMT.                                                
        05 NI-G40-GELDKONTO-TAB.                                                
           10 NI-G40-GELDKONTO-ELE PIC S9(04) COMP OCCURS 20 TIMES.             
        05 NI-G40-GELDKONTO REDEFINES NI-G40-GELDKONTO-TAB.                     
           10 NI-G40GKOVNR          PIC S9(4) COMP.                             
           10 NI-G40GKODATOEFF      PIC S9(4) COMP.                             
           10 NI-G40GKOSCHLDAT      PIC S9(4) COMP.                             
           10 NI-G40GKOBILANZSCHL   PIC S9(4) COMP.                             
           10 NI-G40GKOEXTLIMIT     PIC S9(4) COMP.                             
           10 NI-G40GKVORZEICHEN    PIC S9(4) COMP.                             
           10 NI-G40GKOSALDO        PIC S9(4) COMP.                             
           10 NI-G40GKOKONDSCHL     PIC S9(4) COMP.                             
           10 NI-G40GKODELKZ        PIC S9(4) COMP.                             
           10 NI-G40GKOKUMHABENAM   PIC S9(4) COMP.                             
           10 NI-G40GKOKUMSOLLAM    PIC S9(4) COMP.                             
           10 NI-G40GKOKUMHABENVM   PIC S9(4) COMP.                             
           10 NI-G40GKOKUMSOLLVM    PIC S9(4) COMP.                             
           10 NI-G40GKOZRAM         PIC S9(4) COMP.                             
           10 NI-G40GKOZRVM         PIC S9(4) COMP.                             
           10 NI-G40GKOUEBERZINS    PIC S9(4) COMP.                             
           10 NI-G40GKOHABENZINS    PIC S9(4) COMP.                             
           10 NI-G40GKOSOLLZINS     PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-G40-GELDKONTO.                                                   
           10 G40GKOVNR            PIC X(15).                                   
           10 G40GKODATOEFF        PIC X(8).                                    
           10 G40GKOSCHLDAT        PIC X(8).                                    
           10 G40GKOBILANZSCHL     PIC X(4).                                    
           10 G40GKOEXTLIMIT       PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKVORZEICHEN      PIC X(1).                                    
           10 G40GKOSALDO          PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKOKONDSCHL       PIC X(2).                                    
           10 G40GKODELKZ          PIC X(1).                                    
           10 G40GKOKUMHABENAM     PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKOKUMSOLLAM      PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKOKUMHABENVM     PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKOKUMSOLLVM      PIC S9(12)V9(2) USAGE COMP-3.                
           10 G40GKOZRAM           PIC X(5).                                    
           10 G40GKOZRVM           PIC X(5).                                    
           10 G40GKOUEBERZINS      PIC S9(2)V9(4) USAGE COMP-3.                 
           10 G40GKOHABENZINS      PIC S9(2)V9(4) USAGE COMP-3.                 
           10 G40GKOSOLLZINS       PIC S9(2)V9(4) USAGE COMP-3.                 
           10 LETZTERAENDERER      PIC X(10).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************          
