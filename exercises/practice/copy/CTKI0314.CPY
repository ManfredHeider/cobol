      *****************************************************************         
CCC005*    EXEC SQL DECLARE GUTHABENBUCHUNG TABLE                               
CCC004*    ( GHBVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      GHBGUTHABPRODLFDNR             SMALLINT NOT NULL,                  
CCC004*      GHBFONDID                      SMALLINT NOT NULL,                  
CCC004*      GHBGHALFDNR                    SMALLINT NOT NULL,                  
CCC004*      GHBBUCHUNGLFDNR                SMALLINT NOT NULL,                  
CCC004*      GHBBETRAGLETZTBUCH             DECIMAL(11, 2),                     
CCC004*      GHBBETRLBUWKZ                  CHAR(1) NOT NULL,                   
CCC004*      GHBBUCHUNGSDATUM               DATE,                               
CCC004*      GHBGUTHABENANT                 DECIMAL(11, 4),                     
CCC004*      GHBKZFINHLETZTBUID             SMALLINT,                           
CCC004*      GHBSTEUBETRLETZTBU             DECIMAL(11, 2),                     
CCC004*      GHBSTLETZTBUWKZ                CHAR(1) NOT NULL,                   
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      GHBNETTOBETRAG                 DECIMAL(11, 2) NOT NULL,            
CCC004*      GHBNETTOBETWKZ                 CHAR(1) NOT NULL,                   
CCC004*      GHBGEBBETRAG                   DECIMAL(11, 2),                     
CCC004*      GHBGEBSCHLUESSEL               SMALLINT,                           
CCC004*      GHBGEBTEXT                     CHAR(20)                            
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE GUTHABENBUCHUNG             *               
      ******************************************************************        
       01  GUTHABENBUCHUNG-GESAMT.                                              
        05 NI-GUTHABENBUCHUNG-TAB.                                              
           10 NI-GUTHABENBUCHUNG-ELE PIC S9(04) COMP OCCURS 18 TIMES.           
        05 NI-GUTHABENBUCHUNG REDEFINES NI-GUTHABENBUCHUNG-TAB.                 
           10 NI-GHBVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-GHBGUTHABPRODLFDNR PIC S9(4) COMP.                             
           10 NI-GHBFONDID          PIC S9(4) COMP.                             
           10 NI-GHBGHALFDNR        PIC S9(4) COMP.                             
           10 NI-GHBBUCHUNGLFDNR    PIC S9(4) COMP.                             
           10 NI-GHBBETRAGLETZTBUCH PIC S9(4) COMP.                             
           10 NI-GHBBETRLBUWKZ      PIC S9(4) COMP.                             
           10 NI-GHBBUCHUNGSDATUM   PIC S9(4) COMP.                             
           10 NI-GHBGUTHABENANT     PIC S9(4) COMP.                             
           10 NI-GHBKZFINHLETZTBUID PIC S9(4) COMP.                             
           10 NI-GHBSTEUBETRLETZTBU PIC S9(4) COMP.                             
           10 NI-GHBSTLETZTBUWKZ    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-GHBNETTOBETRAG     PIC S9(4) COMP.                             
           10 NI-GHBNETTOBETWKZ     PIC S9(4) COMP.                             
           10 NI-GHBGEBBETRAG       PIC S9(4) COMP.                             
           10 NI-GHBGEBSCHLUESSEL   PIC S9(4) COMP.                             
           10 NI-GHBGEBTEXT         PIC S9(4) COMP.                             
        05 DCL-GUTHABENBUCHUNG.                                                 
           10 GHBVERTRAGSID        PIC X(13).                                   
           10 GHBGUTHABPRODLFDNR   PIC S9(4) USAGE COMP-5.                      
           10 GHBFONDID            PIC S9(4) USAGE COMP-5.                      
           10 GHBGHALFDNR          PIC S9(4) USAGE COMP-5.                      
           10 GHBBUCHUNGLFDNR      PIC S9(4) USAGE COMP-5.                      
           10 GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHBBETRLBUWKZ        PIC X(1).                                    
           10 GHBBUCHUNGSDATUM     PIC X(10).                                   
           10 GHBGUTHABENANT       PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHBKZFINHLETZTBUID   PIC S9(4) USAGE COMP-5.                      
           10 GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHBSTLETZTBUWKZ      PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 GHBNETTOBETRAG       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHBNETTOBETWKZ       PIC X(1).                                    
           10 GHBGEBBETRAG         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHBGEBSCHLUESSEL     PIC S9(4) USAGE COMP-5.                      
           10 GHBGEBTEXT           PIC X(20).                                   
      ************************** ENDE-DCLGEN *************************          
