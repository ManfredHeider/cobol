      *****************************************************************         
CCC005*    EXEC SQL DECLARE KRANKENPERSEIGEN TABLE                              
CCC004*    ( KPEVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      KPEPERSONLFDNR                 SMALLINT NOT NULL,                  
CCC004*      KPEPRODUKTID                   SMALLINT NOT NULL,                  
CCC004*      KPEANREDE_KTY                  SMALLINT NOT NULL,                  
CCC004*      KPEANREDE_KNR                  SMALLINT NOT NULL,                  
CCC004*      KPENACHNAME                    CHAR(30),                           
CCC004*      KPEVORNAME                     CHAR(30),                           
CCC004*      KPEGEBDATUM                    DATE,                               
CCC004*      KPEPERSONENID                  CHAR(13),                           
CCC004*      KPEADRESSEID                   CHAR(13),                           
CCC004*      KPEBEGINNDATUM                 DATE,                               
CCC004*      KPEENDEDATUM                   DATE,                               
CCC004*      KPEPERSONENNUMMER              SMALLINT,                           
CCC004*      KPEGESCHSTID_KTY               SMALLINT NOT NULL,                  
CCC004*      KPEGESCHSTID_KNR               SMALLINT NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      KPEVORVENDDATUM                DATE,                               
CCC004*      KPEVORVGES_KTY                 SMALLINT,                           
CCC004*      KPEVORVGES_KNR                 DECIMAL(15, 0),                     
CCC004*      KPEVORVKUENDKZ                 CHAR(1),                            
CCC004*      KPEALTRUECKSTELLG              DECIMAL(9, 2),                      
CCC004*      KPEBEITRAGSVORTEIL             DECIMAL(9, 2),                      
CCC004*      KPESTAND                       DATE                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE KRANKENPERSEIGEN            *               
      ******************************************************************        
       01  KRANKENPERSEIGEN-GESAMT.                                             
        05 NI-KRANKENPERSEIGEN-TAB.                                             
           10 NI-KRANKENPERSEIGEN-ELE PIC S9(04) COMP OCCURS 23 TIMES.          
        05 NI-KRANKENPERSEIGEN REDEFINES NI-KRANKENPERSEIGEN-TAB.               
           10 NI-KPEVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-KPEPERSONLFDNR     PIC S9(4) COMP.                             
           10 NI-KPEPRODUKTID       PIC S9(4) COMP.                             
           10 NI-KPEANREDE-KTY      PIC S9(4) COMP.                             
           10 NI-KPEANREDE-KNR      PIC S9(4) COMP.                             
           10 NI-KPENACHNAME        PIC S9(4) COMP.                             
           10 NI-KPEVORNAME         PIC S9(4) COMP.                             
           10 NI-KPEGEBDATUM        PIC S9(4) COMP.                             
           10 NI-KPEPERSONENID      PIC S9(4) COMP.                             
           10 NI-KPEADRESSEID       PIC S9(4) COMP.                             
           10 NI-KPEBEGINNDATUM     PIC S9(4) COMP.                             
           10 NI-KPEENDEDATUM       PIC S9(4) COMP.                             
           10 NI-KPEPERSONENNUMMER  PIC S9(4) COMP.                             
           10 NI-KPEGESCHSTID-KTY   PIC S9(4) COMP.                             
           10 NI-KPEGESCHSTID-KNR   PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-KPEVORVENDDATUM    PIC S9(4) COMP.                             
           10 NI-KPEVORVGES-KTY     PIC S9(4) COMP.                             
           10 NI-KPEVORVGES-KNR     PIC S9(4) COMP.                             
           10 NI-KPEVORVKUENDKZ     PIC S9(4) COMP.                             
           10 NI-KPEALTRUECKSTELLG  PIC S9(4) COMP.                             
           10 NI-KPEBEITRAGSVORTEIL PIC S9(4) COMP.                             
           10 NI-KPESTAND           PIC S9(4) COMP.                             
        05 DCL-KRANKENPERSEIGEN.                                                
           10 KPEVERTRAGSID        PIC X(13).                                   
           10 KPEPERSONLFDNR       PIC S9(4) USAGE COMP.                        
           10 KPEPRODUKTID         PIC S9(4) USAGE COMP.                        
           10 KPEANREDE-KTY        PIC S9(4) USAGE COMP.                        
           10 KPEANREDE-KNR        PIC S9(4) USAGE COMP.                        
           10 KPENACHNAME          PIC X(30).                                   
           10 KPEVORNAME           PIC X(30).                                   
           10 KPEGEBDATUM          PIC X(10).                                   
           10 KPEPERSONENID        PIC X(13).                                   
           10 KPEADRESSEID         PIC X(13).                                   
           10 KPEBEGINNDATUM       PIC X(10).                                   
           10 KPEENDEDATUM         PIC X(10).                                   
           10 KPEPERSONENNUMMER    PIC S9(4) USAGE COMP.                        
           10 KPEGESCHSTID-KTY     PIC S9(4) USAGE COMP.                        
           10 KPEGESCHSTID-KNR     PIC S9(4) USAGE COMP.                        
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 KPEVORVENDDATUM      PIC X(10).                                   
           10 KPEVORVGES-KTY       PIC S9(4) USAGE COMP.                        
           10 KPEVORVGES-KNR       PIC S9(15)V USAGE COMP-3.                    
           10 KPEVORVKUENDKZ       PIC X(1).                                    
           10 KPEALTRUECKSTELLG    PIC S9(7)V9(2) USAGE COMP-3.                 
           10 KPEBEITRAGSVORTEIL   PIC S9(7)V9(2) USAGE COMP-3.                 
           10 KPESTAND             PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
