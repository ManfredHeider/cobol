      *****************************************************************         
CCC005*    EXEC SQL DECLARE VERSLEBENTARIF TABLE                                
CCC004*    ( LTAVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LTAVERSTARLFDNR                SMALLINT NOT NULL,                  
CCC004*      LTARUMSGRUPPE                  CHAR(6),                            
CCC004*      LTARENTENZAHLWID               SMALLINT,                           
CCC004*      LTARENTENGARZEIT               DATE,                               
CCC004*      LTALVART                       SMALLINT,                           
CCC004*      LTAKZKAPWRECHT                 CHAR(1) NOT NULL,                   
CCC004*      LTAFONDSPALETTE                CHAR(8),                            
CCC004*      LTAFONDSAKTKZN                 CHAR(1),                            
CCC004*      LTADEPOTBETRAG                 DECIMAL(13, 2),                     
CCC004*      LTADEPOTZINSEN                 DECIMAL(13, 2),                     
CCC004*      LTADEPOTZINSSATZ               DECIMAL(5, 2),                      
CCC004*      LTADEPWERTTERMIN               DATE,                               
CCC004*      LTAZULAGENART                  SMALLINT,                           
CCC004*      LTADAUZULAGENKZ                CHAR(1),                            
CCC004*      LTASTVD0067_KTY                SMALLINT,                           
CCC004*      LTASTVD0067_KNR                DECIMAL(15, 0),                     
CCC004*      LTADIREKTVER_KTY               SMALLINT,                           
CCC004*      LTADIREKTVER_KNR               SMALLINT,                           
CCC004*      LTAABTRETUNGKZ                 CHAR(1),                            
CCC004*      LTARENTENBEZUG                 SMALLINT,                           
CCC004*      LTABULEISTUNG                  CHAR(1),                            
CCC004*      LTANETTOBEITRAG                DECIMAL(11, 2),                     
CCC004*      LTABONUSKZ                     CHAR(1),                            
CCC004*      LTAKONVANLPROZ                 DECIMAL(3, 0),                      
CCC004*      LTATSTHOST                     TIMESTAMP,                          
CCC004*      LTAPRODBEZ_KTY                 SMALLINT,                           
CCC004*      LTAPRODBEZ_KNR                 DECIMAL(15, 0),                     
CCC004*      LTALVURSPRBEG                  DATE,                               
CCC004*      LTAURSPRPOLDAT                 DATE,                               
CCC004*      LTAFONDSMANKZ                  CHAR(1) NOT NULL,                   
CCC004*      LTAFOMGNTKWAB                  DATE,                               
CCC004*      LTAFOMGNTKWBIS                 DATE,                               
CCC004*      LTABUKTKZN                     CHAR(1) NOT NULL,                   
CCC004*      LTAVITALITYKZN_KTY             SMALLINT,                           
CCC004*      LTAVITALITYKZN_KNR             DECIMAL(15, 0),                     
CCC004*      LTANEUABSCHLKZN                CHAR(1) NOT NULL,                   
CCC004*      LTABASISZINS_BETR              DECIMAL(15, 2),                     
CCC004*      LTAZINSBOOST_SATZ              DECIMAL(9, 3),                      
CCC004*      LTAZINSBOOST_BETR              DECIMAL(15, 2),                     
CCC004*      LTAEUKLAUSELKZN                CHAR(1) NOT NULL,                   
CCC004*      LTASCHUELER_BU                 CHAR(1) NOT NULL,                   
CCC004*      LTABTG_AG                      DECIMAL(13, 2) NOT NULL,            
CCC004*      LTABTG_AN                      DECIMAL(13, 2) NOT NULL,            
CCC004*      LTAKLBERHAUSSCHL               CHAR(1) NOT NULL,                   
CCC004*      LTAWERTSICHOPTKZN              CHAR(1) NOT NULL                    
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VERSLEBENTARIF              *               
      ******************************************************************        
       01  VERSLEBENTARIF-GESAMT.                                               
        05 NI-VERSLEBENTARIF-TAB.                                               
           10 NI-VERSLEBENTARIF-ELE PIC S9(04) COMP OCCURS 46 TIMES.            
        05 NI-VERSLEBENTARIF REDEFINES NI-VERSLEBENTARIF-TAB.                   
           10 NI-LTAVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LTAVERSTARLFDNR    PIC S9(4) COMP.                             
           10 NI-LTARUMSGRUPPE      PIC S9(4) COMP.                             
           10 NI-LTARENTENZAHLWID   PIC S9(4) COMP.                             
           10 NI-LTARENTENGARZEIT   PIC S9(4) COMP.                             
           10 NI-LTALVART           PIC S9(4) COMP.                             
           10 NI-LTAKZKAPWRECHT     PIC S9(4) COMP.                             
           10 NI-LTAFONDSPALETTE    PIC S9(4) COMP.                             
           10 NI-LTAFONDSAKTKZN     PIC S9(4) COMP.                             
           10 NI-LTADEPOTBETRAG     PIC S9(4) COMP.                             
           10 NI-LTADEPOTZINSEN     PIC S9(4) COMP.                             
           10 NI-LTADEPOTZINSSATZ   PIC S9(4) COMP.                             
           10 NI-LTADEPWERTTERMIN   PIC S9(4) COMP.                             
           10 NI-LTAZULAGENART      PIC S9(4) COMP.                             
           10 NI-LTADAUZULAGENKZ    PIC S9(4) COMP.                             
           10 NI-LTASTVD0067-KTY    PIC S9(4) COMP.                             
           10 NI-LTASTVD0067-KNR    PIC S9(4) COMP.                             
           10 NI-LTADIREKTVER-KTY   PIC S9(4) COMP.                             
           10 NI-LTADIREKTVER-KNR   PIC S9(4) COMP.                             
           10 NI-LTAABTRETUNGKZ     PIC S9(4) COMP.                             
           10 NI-LTARENTENBEZUG     PIC S9(4) COMP.                             
           10 NI-LTABULEISTUNG      PIC S9(4) COMP.                             
           10 NI-LTANETTOBEITRAG    PIC S9(4) COMP.                             
           10 NI-LTABONUSKZ         PIC S9(4) COMP.                             
           10 NI-LTAKONVANLPROZ     PIC S9(4) COMP.                             
           10 NI-LTATSTHOST         PIC S9(4) COMP.                             
           10 NI-LTAPRODBEZ-KTY     PIC S9(4) COMP.                             
           10 NI-LTAPRODBEZ-KNR     PIC S9(4) COMP.                             
           10 NI-LTALVURSPRBEG      PIC S9(4) COMP.                             
           10 NI-LTAURSPRPOLDAT     PIC S9(4) COMP.                             
           10 NI-LTAFONDSMANKZ      PIC S9(4) COMP.                             
           10 NI-LTAFOMGNTKWAB      PIC S9(4) COMP.                             
           10 NI-LTAFOMGNTKWBIS     PIC S9(4) COMP.                             
           10 NI-LTABUKTKZN         PIC S9(4) COMP.                             
           10 NI-LTAVITALITYKZN-KTY PIC S9(4) COMP.                             
           10 NI-LTAVITALITYKZN-KNR PIC S9(4) COMP.                             
           10 NI-LTANEUABSCHLKZN    PIC S9(4) COMP.                             
           10 NI-LTABASISZINS-BETR  PIC S9(4) COMP.                             
           10 NI-LTAZINSBOOST-SATZ  PIC S9(4) COMP.                             
           10 NI-LTAZINSBOOST-BETR  PIC S9(4) COMP.                             
           10 NI-LTAEUKLAUSELKZN    PIC S9(4) COMP.                             
           10 NI-LTASCHUELER-BU     PIC S9(4) COMP.                             
           10 NI-LTABTG-AG          PIC S9(4) COMP.                             
           10 NI-LTABTG-AN          PIC S9(4) COMP.                             
           10 NI-LTAKLBERHAUSSCHL   PIC S9(4) COMP.                             
           10 NI-LTAWERTSICHOPTKZN  PIC S9(4) COMP.                             
        05 DCL-VERSLEBENTARIF.                                                  
           10 LTAVERTRAGSID        PIC X(13).                                   
           10 LTAVERSTARLFDNR      PIC S9(4) USAGE COMP-5.                      
           10 LTARUMSGRUPPE        PIC X(6).                                    
           10 LTARENTENZAHLWID     PIC S9(4) USAGE COMP-5.                      
           10 LTARENTENGARZEIT     PIC X(10).                                   
           10 LTALVART             PIC S9(4) USAGE COMP-5.                      
           10 LTAKZKAPWRECHT       PIC X(1).                                    
           10 LTAFONDSPALETTE      PIC X(8).                                    
           10 LTAFONDSAKTKZN       PIC X(1).                                    
           10 LTADEPOTBETRAG       PIC S9(11)V9(2) USAGE COMP-3.                
           10 LTADEPOTZINSEN       PIC S9(11)V9(2) USAGE COMP-3.                
           10 LTADEPOTZINSSATZ     PIC S9(3)V9(2) USAGE COMP-3.                 
           10 LTADEPWERTTERMIN     PIC X(10).                                   
           10 LTAZULAGENART        PIC S9(4) USAGE COMP-5.                      
           10 LTADAUZULAGENKZ      PIC X(1).                                    
           10 LTASTVD0067-KTY      PIC S9(4) USAGE COMP-5.                      
           10 LTASTVD0067-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 LTADIREKTVER-KTY     PIC S9(4) USAGE COMP-5.                      
           10 LTADIREKTVER-KNR     PIC S9(4) USAGE COMP-5.                      
           10 LTAABTRETUNGKZ       PIC X(1).                                    
           10 LTARENTENBEZUG       PIC S9(4) USAGE COMP-5.                      
           10 LTABULEISTUNG        PIC X(1).                                    
           10 LTANETTOBEITRAG      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 LTABONUSKZ           PIC X(1).                                    
           10 LTAKONVANLPROZ       PIC S9(3)V USAGE COMP-3.                     
           10 LTATSTHOST           PIC X(26).                                   
           10 LTAPRODBEZ-KTY       PIC S9(4) USAGE COMP-5.                      
           10 LTAPRODBEZ-KNR       PIC S9(15)V USAGE COMP-3.                    
           10 LTALVURSPRBEG        PIC X(10).                                   
           10 LTAURSPRPOLDAT       PIC X(10).                                   
           10 LTAFONDSMANKZ        PIC X(1).                                    
           10 LTAFOMGNTKWAB        PIC X(10).                                   
           10 LTAFOMGNTKWBIS       PIC X(10).                                   
           10 LTABUKTKZN           PIC X(1).                                    
           10 LTAVITALITYKZN-KTY   PIC S9(4) USAGE COMP-5.                      
           10 LTAVITALITYKZN-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 LTANEUABSCHLKZN      PIC X(1).                                    
           10 LTABASISZINS-BETR    PIC S9(13)V9(2) USAGE COMP-3.                
           10 LTAZINSBOOST-SATZ    PIC S9(6)V9(3) USAGE COMP-3.                 
           10 LTAZINSBOOST-BETR    PIC S9(13)V9(2) USAGE COMP-3.                
           10 LTAEUKLAUSELKZN      PIC X(1).                                    
           10 LTASCHUELER-BU       PIC X(1).                                    
           10 LTABTG-AG            PIC S9(11)V9(2) USAGE COMP-3.                
           10 LTABTG-AN            PIC S9(11)V9(2) USAGE COMP-3.                
           10 LTAKLBERHAUSSCHL     PIC X(1).                                    
           10 LTAWERTSICHOPTKZN    PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
