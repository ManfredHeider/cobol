      *****************************************************************         
CCC005*    EXEC SQL DECLARE GUTHABENANTEIL TABLE                                
CCC004*    ( GHAVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      GHAGUTHABPRODLFDNR             SMALLINT NOT NULL,                  
CCC004*      GHAFONDID                      SMALLINT NOT NULL,                  
CCC004*      GHALFDNR                       SMALLINT NOT NULL,                  
CCC004*      GHAGUTHABEN                    DECIMAL(11, 2),                     
CCC004*      GHAGUTHABENWKZ                 CHAR(1) NOT NULL,                   
CCC004*      GHAGUTHABENANT                 DECIMAL(11, 4),                     
CCC004*      GHAKUMULAUSZAHLUNG             DECIMAL(13, 0),                     
CCC004*      GHAKUMULEINZAHLUNG             DECIMAL(13, 0),                     
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      GHALIEFERDATUM                 DATE,                               
CCC004*      GHAKUMAUTOWA                   DECIMAL(13, 2),                     
CCC004*      GHAHISEINZAHLUNG               DECIMAL(13, 2),                     
CCC004*      GHAHISAUSZAHLUNG               DECIMAL(13, 2),                     
CCC004*      GHAKAPITALAKT                  DECIMAL(11, 2),                     
CCC004*      GHAKAPITALVORJ                 DECIMAL(11, 2),                     
CCC004*      GHAKUMUEBERTRGES               DECIMAL(11, 2),                     
CCC004*      GHAKUMUEBERTRVOR               DECIMAL(11, 2),                     
CCC004*      GHAEINSTANDSKURS               DECIMAL(11, 2),                     
CCC004*      GHAFLAGEINSTKURS               CHAR(1),                            
CCC004*      GHAREALGEWVERL                 DECIMAL(11, 2),                     
CCC004*      GHAGUTHABENVORJ                DECIMAL(11, 2),                     
CCC004*      GHAKUMBARAUSBR                 DECIMAL(11, 2),                     
CCC004*      GHAANTEILEVORJ                 DECIMAL(10, 4),                     
CCC004*      GHAANTEILEFREI                 DECIMAL(11, 4),                     
CCC004*      GHAVLSPERRDAT1                 DATE,                               
CCC004*      GHAANTGESPERRT1                DECIMAL(11, 4),                     
CCC004*      GHAVLSPERRDAT2                 DATE,                               
CCC004*      GHAANTGESPERRT2                DECIMAL(11, 4),                     
CCC004*      GHAVLSPERRDAT3                 DATE,                               
CCC004*      GHAANTGESPERRT3                DECIMAL(11, 4),                     
CCC004*      GHARUECKBRUGES                 DECIMAL(11, 2),                     
CCC004*      GHARUECKBRUVORJ                DECIMAL(11, 2),                     
CCC004*      GHAKUMBARAUSBRVJ               DECIMAL(11, 2),                     
CCC004*      GHAANTEILGEKOIF                DECIMAL(11, 4),                     
CCC004*      GHAKUMAUSZHALBJ                DECIMAL(11, 2),                     
CCC004*      GHAANTEILVEFOIF                DECIMAL(11, 4),                     
CCC004*      GHAANTEILGESPOIF               DECIMAL(11, 4),                     
CCC004*      GHAANTERWVORJU13               DECIMAL(11, 4),                     
CCC004*      GHAANLEGERIDOIF                CHAR(14),                           
CCC004*      GHAWPBEZ_KTY                   SMALLINT,                           
CCC004*      GHAWPBEZ_KNR                   DECIMAL(15, 0),                     
CCC004*      GHARUECKNAHMEPR                DECIMAL(9, 4),                      
CCC004*      GHANRGWINKLBBA                 DECIMAL(13, 2),                     
CCC004*      GHANRGWINKLBBAVH               DECIMAL(8, 2),                      
CCC004*      GHANRGWOHNEBBA                 DECIMAL(13, 2),                     
CCC004*      GHANRGWOHNEBBAVH               DECIMAL(8, 2)                       
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE GUTHABENANTEIL              *               
      ******************************************************************        
       01  GUTHABENANTEIL-GESAMT.                                               
        05 NI-GUTHABENANTEIL-TAB.                                               
           10 NI-GUTHABENANTEIL-ELE PIC S9(04) COMP OCCURS 47 TIMES.            
        05 NI-GUTHABENANTEIL REDEFINES NI-GUTHABENANTEIL-TAB.                   
           10 NI-GHAVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-GHAGUTHABPRODLFDNR PIC S9(4) COMP.                             
           10 NI-GHAFONDID          PIC S9(4) COMP.                             
           10 NI-GHALFDNR           PIC S9(4) COMP.                             
           10 NI-GHAGUTHABEN        PIC S9(4) COMP.                             
           10 NI-GHAGUTHABENWKZ     PIC S9(4) COMP.                             
           10 NI-GHAGUTHABENANT     PIC S9(4) COMP.                             
           10 NI-GHAKUMULAUSZAHLUNG PIC S9(4) COMP.                             
           10 NI-GHAKUMULEINZAHLUNG PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-GHALIEFERDATUM     PIC S9(4) COMP.                             
           10 NI-GHAKUMAUTOWA       PIC S9(4) COMP.                             
           10 NI-GHAHISEINZAHLUNG   PIC S9(4) COMP.                             
           10 NI-GHAHISAUSZAHLUNG   PIC S9(4) COMP.                             
           10 NI-GHAKAPITALAKT      PIC S9(4) COMP.                             
           10 NI-GHAKAPITALVORJ     PIC S9(4) COMP.                             
           10 NI-GHAKUMUEBERTRGES   PIC S9(4) COMP.                             
           10 NI-GHAKUMUEBERTRVOR   PIC S9(4) COMP.                             
           10 NI-GHAEINSTANDSKURS   PIC S9(4) COMP.                             
           10 NI-GHAFLAGEINSTKURS   PIC S9(4) COMP.                             
           10 NI-GHAREALGEWVERL     PIC S9(4) COMP.                             
           10 NI-GHAGUTHABENVORJ    PIC S9(4) COMP.                             
           10 NI-GHAKUMBARAUSBR     PIC S9(4) COMP.                             
           10 NI-GHAANTEILEVORJ     PIC S9(4) COMP.                             
           10 NI-GHAANTEILEFREI     PIC S9(4) COMP.                             
           10 NI-GHAVLSPERRDAT1     PIC S9(4) COMP.                             
           10 NI-GHAANTGESPERRT1    PIC S9(4) COMP.                             
           10 NI-GHAVLSPERRDAT2     PIC S9(4) COMP.                             
           10 NI-GHAANTGESPERRT2    PIC S9(4) COMP.                             
           10 NI-GHAVLSPERRDAT3     PIC S9(4) COMP.                             
           10 NI-GHAANTGESPERRT3    PIC S9(4) COMP.                             
           10 NI-GHARUECKBRUGES     PIC S9(4) COMP.                             
           10 NI-GHARUECKBRUVORJ    PIC S9(4) COMP.                             
           10 NI-GHAKUMBARAUSBRVJ   PIC S9(4) COMP.                             
           10 NI-GHAANTEILGEKOIF    PIC S9(4) COMP.                             
           10 NI-GHAKUMAUSZHALBJ    PIC S9(4) COMP.                             
           10 NI-GHAANTEILVEFOIF    PIC S9(4) COMP.                             
           10 NI-GHAANTEILGESPOIF   PIC S9(4) COMP.                             
           10 NI-GHAANTERWVORJU13   PIC S9(4) COMP.                             
           10 NI-GHAANLEGERIDOIF    PIC S9(4) COMP.                             
           10 NI-GHAWPBEZ-KTY       PIC S9(4) COMP.                             
           10 NI-GHAWPBEZ-KNR       PIC S9(4) COMP.                             
           10 NI-GHARUECKNAHMEPR    PIC S9(4) COMP.                             
           10 NI-GHANRGWINKLBBA     PIC S9(4) COMP.                             
           10 NI-GHANRGWINKLBBAVH   PIC S9(4) COMP.                             
           10 NI-GHANRGWOHNEBBA     PIC S9(4) COMP.                             
           10 NI-GHANRGWOHNEBBAVH   PIC S9(4) COMP.                             
        05 DCL-GUTHABENANTEIL.                                                  
           10 GHAVERTRAGSID        PIC X(13).                                   
           10 GHAGUTHABPRODLFDNR   PIC S9(4) USAGE COMP-5.                      
           10 GHAFONDID            PIC S9(4) USAGE COMP-5.                      
           10 GHALFDNR             PIC S9(4) USAGE COMP-5.                      
           10 GHAGUTHABEN          PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAGUTHABENWKZ       PIC X(1).                                    
           10 GHAGUTHABENANT       PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAKUMULAUSZAHLUNG   PIC S9(13)V USAGE COMP-3.                    
           10 GHAKUMULEINZAHLUNG   PIC S9(13)V USAGE COMP-3.                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 GHALIEFERDATUM       PIC X(10).                                   
           10 GHAKUMAUTOWA         PIC S9(11)V9(2) USAGE COMP-3.                
           10 GHAHISEINZAHLUNG     PIC S9(11)V9(2) USAGE COMP-3.                
           10 GHAHISAUSZAHLUNG     PIC S9(11)V9(2) USAGE COMP-3.                
           10 GHAKAPITALAKT        PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAKAPITALVORJ       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAKUMUEBERTRGES     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAKUMUEBERTRVOR     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAEINSTANDSKURS     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAFLAGEINSTKURS     PIC X(1).                                    
           10 GHAREALGEWVERL       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAGUTHABENVORJ      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAKUMBARAUSBR       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAANTEILEVORJ       PIC S9(6)V9(4) USAGE COMP-3.                 
           10 GHAANTEILEFREI       PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAVLSPERRDAT1       PIC X(10).                                   
           10 GHAANTGESPERRT1      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAVLSPERRDAT2       PIC X(10).                                   
           10 GHAANTGESPERRT2      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAVLSPERRDAT3       PIC X(10).                                   
           10 GHAANTGESPERRT3      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHARUECKBRUGES       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHARUECKBRUVORJ      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAKUMBARAUSBRVJ     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAANTEILGEKOIF      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAKUMAUSZHALBJ      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GHAANTEILVEFOIF      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAANTEILGESPOIF     PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAANTERWVORJU13     PIC S9(7)V9(4) USAGE COMP-3.                 
           10 GHAANLEGERIDOIF      PIC X(14).                                   
           10 GHAWPBEZ-KTY         PIC S9(4) USAGE COMP-5.                      
           10 GHAWPBEZ-KNR         PIC S9(15)V USAGE COMP-3.                    
           10 GHARUECKNAHMEPR      PIC S9(5)V9(4) USAGE COMP-3.                 
           10 GHANRGWINKLBBA       PIC S9(11)V9(2) USAGE COMP-3.                
           10 GHANRGWINKLBBAVH     PIC S9(6)V9(2) USAGE COMP-3.                 
           10 GHANRGWOHNEBBA       PIC S9(11)V9(2) USAGE COMP-3.                
           10 GHANRGWOHNEBBAVH     PIC S9(6)V9(2) USAGE COMP-3.                 
      ************************** ENDE-DCLGEN *************************          
