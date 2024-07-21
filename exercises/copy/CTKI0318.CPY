      *****************************************************************         
CCC005*    EXEC SQL DECLARE KFZ TABLE                                           
CCC004*    ( KFZVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      KFZVERSTARLFDNR                SMALLINT NOT NULL,                  
CCC004*      KFZAMTLICHESKENNZ              CHAR(12),                           
CCC004*      KFZFAHRGESTELLNR               CHAR(20),                           
CCC004*      KFZGESAMTNEUWERTFZ             DECIMAL(11, 2),                     
CCC004*      KFZGESNEUWERTWKZ               CHAR(1) NOT NULL,                   
CCC004*      KFZHAFTPFLTYPKL                SMALLINT,                           
CCC004*      KFZHERSTELLERSCHL              CHAR(4),                            
CCC004*      KFZSCHLUESSELTYP               CHAR(3),                            
CCC004*      KFZHERSTMODTEXT                CHAR(20),                           
CCC004*      KFZLEISTUNG                    DECIMAL(7, 2),                      
CCC004*      KFZLEISTUNGSARTID              SMALLINT,                           
CCC004*      KFZLEISTUNGTEXT                CHAR(12),                           
CCC004*      KFZVOLLKASKOTYPKL              SMALLINT,                           
CCC004*      KFZZULASSUNGDATUM              DATE,                               
CCC004*      KFZSCHUTZBRIEF                 CHAR(10),                           
CCC004*      KFZFAHRZEUGTYPID               SMALLINT,                           
CCC004*      KFZSAISONVON                   SMALLINT,                           
CCC004*      KFZSAISONBIS                   SMALLINT,                           
CCC004*      KFZSAISONKZ                    CHAR(1) NOT NULL,                   
CCC004*      KFZERSTZULDATUMVN              DATE,                               
CCC004*      KFZLSVSCOR                     CHAR(1),                            
CCC004*      KFZHALTERNNAME                 CHAR(40),                           
CCC004*      KFZHALTERVNAME                 CHAR(40),                           
CCC004*      KFZFAHRGEBIET                  CHAR(10),                           
CCC004*      KFZFSERWERB                    CHAR(10),                           
CCC004*      KFZWOHNEIGT                    CHAR(10),                           
CCC004*      KFZFAHRER                      CHAR(10),                           
CCC004*      KFZGEBDATFAHRER                DATE,                               
CCC004*      KFZGEBDATFAHRERIN              DATE,                               
CCC004*      KFZFZGNUTZUNG                  CHAR(10),                           
CCC004*      KFZFZGABSTELLPLATZ             CHAR(10),                           
CCC004*      KFZKMSTAND                     INTEGER,                            
CCC004*      KFZJAEHRLFAHRL                 INTEGER,                            
CCC004*      KFZAGBRANCHE                   CHAR(60),                           
CCC004*      KFZWERKSTBINDKZ                CHAR(1),                            
CCC004*      KFZFZGART_KTY                  SMALLINT,                           
CCC004*      KFZFZGART_KNR                  DECIMAL(15, 0),                     
CCC004*      KFZVERWEND_KTY                 SMALLINT,                           
CCC004*      KFZVERWEND_KNR                 DECIMAL(15, 0),                     
CCC004*      KFZNAEHSPEZ_KTY                SMALLINT,                           
CCC004*      KFZNAEHSPEZ_KNR                DECIMAL(15, 0),                     
CCC004*      KFZAKZART_KTY                  SMALLINT,                           
CCC004*      KFZAKZART_KNR                  DECIMAL(15, 0),                     
CCC004*      KFZSFANZKH                     SMALLINT,                           
CCC004*      KFZSFANZVK                     SMALLINT,                           
CCC004*      KFZTARIFAUSWAHL                CHAR(1),                            
CCC004*      KFZAUFBAUART_KTY               SMALLINT,                           
CCC004*      KFZAUFBAUART_KNR               DECIMAL(15, 0),                     
CCC004*      KFZHERSTELLTEXT                CHAR(25),                           
CCC004*      KFZALTERSGRP_KTY               SMALLINT,                           
CCC004*      KFZALTERSGRP_KNR               DECIMAL(15, 0),                     
CCC004*      KFZLEASING                     SMALLINT,                           
CCC004*      KFZBEITRANPASSAM               DATE,                               
CCC004*      KFZBERECHNETAM                 DATE,                               
CCC004*      KFZBEITRGESAMNEU               DECIMAL(7, 2),                      
CCC004*      KFZBEITRGESAMALT               DECIMAL(7, 2),                      
CCC004*      KFZKMSTANDAKT                  DECIMAL(7, 0),                      
CCC004*      KFZDATKMSTANDAKT               DATE,                               
CCC004*      KFZABWFAHRLSTJH                DECIMAL(7, 0),                      
CCC004*      KFZSFRFREIKH                   CHAR(1),                            
CCC004*      KFZSFRFREIVK                   CHAR(1),                            
CCC004*      KFZPLATZ                       DECIMAL(3, 0),                      
CCC004*      KFZGESGEW                      DECIMAL(7, 0),                      
CCC004*      KFZKW                          DECIMAL(4, 0),                      
CCC004*      KFZNUTZ                        DECIMAL(7, 0),                      
CCC004*      KFZHALTERSCHL                  SMALLINT,                           
CCC004*      KFZHALTERTEXT                  CHAR(10),                           
CCC004*      KFZBEGFAHRERSCHL               SMALLINT,                           
CCC004*      KFZBEGFAHRERTEXT               CHAR(10),                           
CCC004*      KFZPSFANZKH                    SMALLINT,                           
CCC004*      KFZPSFANZVK                    SMALLINT,                           
CCC004*      KFZPAKET_KTY                   SMALLINT,                           
CCC004*      KFZPAKET_KNR                   DECIMAL(15, 0),                     
CCC004*      KFZSCOREWERT                   DECIMAL(3, 0),                      
CCC004*      KFZSCOREDATUM                  DATE,                               
CCC004*      KFZTELEMATIKID                 DECIMAL(14, 0),                     
CCC004*      KFZFALTERMAX_KTY               SMALLINT,                           
CCC004*      KFZFALTERMAX_KNR               DECIMAL(15, 0),                     
CCC004*      KFZ_E_FZG_KZ                   CHAR(1),                            
CCC004*      KFZFIRMENNR                    CHAR(6),                            
CCC004*      KFZGEBDAT_VN                   DATE                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE KFZ                         *               
      ******************************************************************        
       01  KFZ-GESAMT.                                                          
        05 NI-KFZ-TAB.                                                          
           10 NI-KFZ-ELE PIC S9(04) COMP OCCURS 82 TIMES.                       
        05 NI-KFZ REDEFINES NI-KFZ-TAB.                                         
           10 NI-KFZVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-KFZVERSTARLFDNR    PIC S9(4) COMP.                             
           10 NI-KFZAMTLICHESKENNZ  PIC S9(4) COMP.                             
           10 NI-KFZFAHRGESTELLNR   PIC S9(4) COMP.                             
           10 NI-KFZGESAMTNEUWERTFZ PIC S9(4) COMP.                             
           10 NI-KFZGESNEUWERTWKZ   PIC S9(4) COMP.                             
           10 NI-KFZHAFTPFLTYPKL    PIC S9(4) COMP.                             
           10 NI-KFZHERSTELLERSCHL  PIC S9(4) COMP.                             
           10 NI-KFZSCHLUESSELTYP   PIC S9(4) COMP.                             
           10 NI-KFZHERSTMODTEXT    PIC S9(4) COMP.                             
           10 NI-KFZLEISTUNG        PIC S9(4) COMP.                             
           10 NI-KFZLEISTUNGSARTID  PIC S9(4) COMP.                             
           10 NI-KFZLEISTUNGTEXT    PIC S9(4) COMP.                             
           10 NI-KFZVOLLKASKOTYPKL  PIC S9(4) COMP.                             
           10 NI-KFZZULASSUNGDATUM  PIC S9(4) COMP.                             
           10 NI-KFZSCHUTZBRIEF     PIC S9(4) COMP.                             
           10 NI-KFZFAHRZEUGTYPID   PIC S9(4) COMP.                             
           10 NI-KFZSAISONVON       PIC S9(4) COMP.                             
           10 NI-KFZSAISONBIS       PIC S9(4) COMP.                             
           10 NI-KFZSAISONKZ        PIC S9(4) COMP.                             
           10 NI-KFZERSTZULDATUMVN  PIC S9(4) COMP.                             
           10 NI-KFZLSVSCOR         PIC S9(4) COMP.                             
           10 NI-KFZHALTERNNAME     PIC S9(4) COMP.                             
           10 NI-KFZHALTERVNAME     PIC S9(4) COMP.                             
           10 NI-KFZFAHRGEBIET      PIC S9(4) COMP.                             
           10 NI-KFZFSERWERB        PIC S9(4) COMP.                             
           10 NI-KFZWOHNEIGT        PIC S9(4) COMP.                             
           10 NI-KFZFAHRER          PIC S9(4) COMP.                             
           10 NI-KFZGEBDATFAHRER    PIC S9(4) COMP.                             
           10 NI-KFZGEBDATFAHRERIN  PIC S9(4) COMP.                             
           10 NI-KFZFZGNUTZUNG      PIC S9(4) COMP.                             
           10 NI-KFZFZGABSTELLPLATZ PIC S9(4) COMP.                             
           10 NI-KFZKMSTAND         PIC S9(4) COMP.                             
           10 NI-KFZJAEHRLFAHRL     PIC S9(4) COMP.                             
           10 NI-KFZAGBRANCHE       PIC S9(4) COMP.                             
           10 NI-KFZWERKSTBINDKZ    PIC S9(4) COMP.                             
           10 NI-KFZFZGART-KTY      PIC S9(4) COMP.                             
           10 NI-KFZFZGART-KNR      PIC S9(4) COMP.                             
           10 NI-KFZVERWEND-KTY     PIC S9(4) COMP.                             
           10 NI-KFZVERWEND-KNR     PIC S9(4) COMP.                             
           10 NI-KFZNAEHSPEZ-KTY    PIC S9(4) COMP.                             
           10 NI-KFZNAEHSPEZ-KNR    PIC S9(4) COMP.                             
           10 NI-KFZAKZART-KTY      PIC S9(4) COMP.                             
           10 NI-KFZAKZART-KNR      PIC S9(4) COMP.                             
           10 NI-KFZSFANZKH         PIC S9(4) COMP.                             
           10 NI-KFZSFANZVK         PIC S9(4) COMP.                             
           10 NI-KFZTARIFAUSWAHL    PIC S9(4) COMP.                             
           10 NI-KFZAUFBAUART-KTY   PIC S9(4) COMP.                             
           10 NI-KFZAUFBAUART-KNR   PIC S9(4) COMP.                             
           10 NI-KFZHERSTELLTEXT    PIC S9(4) COMP.                             
           10 NI-KFZALTERSGRP-KTY   PIC S9(4) COMP.                             
           10 NI-KFZALTERSGRP-KNR   PIC S9(4) COMP.                             
           10 NI-KFZLEASING         PIC S9(4) COMP.                             
           10 NI-KFZBEITRANPASSAM   PIC S9(4) COMP.                             
           10 NI-KFZBERECHNETAM     PIC S9(4) COMP.                             
           10 NI-KFZBEITRGESAMNEU   PIC S9(4) COMP.                             
           10 NI-KFZBEITRGESAMALT   PIC S9(4) COMP.                             
           10 NI-KFZKMSTANDAKT      PIC S9(4) COMP.                             
           10 NI-KFZDATKMSTANDAKT   PIC S9(4) COMP.                             
           10 NI-KFZABWFAHRLSTJH    PIC S9(4) COMP.                             
           10 NI-KFZSFRFREIKH       PIC S9(4) COMP.                             
           10 NI-KFZSFRFREIVK       PIC S9(4) COMP.                             
           10 NI-KFZPLATZ           PIC S9(4) COMP.                             
           10 NI-KFZGESGEW          PIC S9(4) COMP.                             
           10 NI-KFZKW              PIC S9(4) COMP.                             
           10 NI-KFZNUTZ            PIC S9(4) COMP.                             
           10 NI-KFZHALTERSCHL      PIC S9(4) COMP.                             
           10 NI-KFZHALTERTEXT      PIC S9(4) COMP.                             
           10 NI-KFZBEGFAHRERSCHL   PIC S9(4) COMP.                             
           10 NI-KFZBEGFAHRERTEXT   PIC S9(4) COMP.                             
           10 NI-KFZPSFANZKH        PIC S9(4) COMP.                             
           10 NI-KFZPSFANZVK        PIC S9(4) COMP.                             
           10 NI-KFZPAKET-KTY       PIC S9(4) COMP.                             
           10 NI-KFZPAKET-KNR       PIC S9(4) COMP.                             
           10 NI-KFZSCOREWERT       PIC S9(4) COMP.                             
           10 NI-KFZSCOREDATUM      PIC S9(4) COMP.                             
           10 NI-KFZTELEMATIKID     PIC S9(4) COMP.                             
           10 NI-KFZFALTERMAX-KTY   PIC S9(4) COMP.                             
           10 NI-KFZFALTERMAX-KNR   PIC S9(4) COMP.                             
           10 NI-KFZ-E-FZG-KZ       PIC S9(4) COMP.                             
           10 NI-KFZFIRMENNR        PIC S9(4) COMP.                             
           10 NI-KFZGEBDAT-VN       PIC S9(4) COMP.                             
        05 DCL-KFZ.                                                             
           10 KFZVERTRAGSID        PIC X(13).                                   
           10 KFZVERSTARLFDNR      PIC S9(4) USAGE COMP.                        
           10 KFZAMTLICHESKENNZ    PIC X(12).                                   
           10 KFZFAHRGESTELLNR     PIC X(20).                                   
           10 KFZGESAMTNEUWERTFZ   PIC S9(9)V9(2) USAGE COMP-3.                 
           10 KFZGESNEUWERTWKZ     PIC X(1).                                    
           10 KFZHAFTPFLTYPKL      PIC S9(4) USAGE COMP.                        
           10 KFZHERSTELLERSCHL    PIC X(4).                                    
           10 KFZSCHLUESSELTYP     PIC X(3).                                    
           10 KFZHERSTMODTEXT      PIC X(20).                                   
           10 KFZLEISTUNG          PIC S9(5)V9(2) USAGE COMP-3.                 
           10 KFZLEISTUNGSARTID    PIC S9(4) USAGE COMP.                        
           10 KFZLEISTUNGTEXT      PIC X(12).                                   
           10 KFZVOLLKASKOTYPKL    PIC S9(4) USAGE COMP.                        
           10 KFZZULASSUNGDATUM    PIC X(10).                                   
           10 KFZSCHUTZBRIEF       PIC X(10).                                   
           10 KFZFAHRZEUGTYPID     PIC S9(4) USAGE COMP.                        
           10 KFZSAISONVON         PIC S9(4) USAGE COMP.                        
           10 KFZSAISONBIS         PIC S9(4) USAGE COMP.                        
           10 KFZSAISONKZ          PIC X(1).                                    
           10 KFZERSTZULDATUMVN    PIC X(10).                                   
           10 KFZLSVSCOR           PIC X(1).                                    
           10 KFZHALTERNNAME       PIC X(40).                                   
           10 KFZHALTERVNAME       PIC X(40).                                   
           10 KFZFAHRGEBIET        PIC X(10).                                   
           10 KFZFSERWERB          PIC X(10).                                   
           10 KFZWOHNEIGT          PIC X(10).                                   
           10 KFZFAHRER            PIC X(10).                                   
           10 KFZGEBDATFAHRER      PIC X(10).                                   
           10 KFZGEBDATFAHRERIN    PIC X(10).                                   
           10 KFZFZGNUTZUNG        PIC X(10).                                   
           10 KFZFZGABSTELLPLATZ   PIC X(10).                                   
           10 KFZKMSTAND           PIC S9(9) USAGE COMP.                        
           10 KFZJAEHRLFAHRL       PIC S9(9) USAGE COMP.                        
           10 KFZAGBRANCHE         PIC X(60).                                   
           10 KFZWERKSTBINDKZ      PIC X(1).                                    
           10 KFZFZGART-KTY        PIC S9(4) USAGE COMP.                        
           10 KFZFZGART-KNR        PIC S9(15)V USAGE COMP-3.                    
           10 KFZVERWEND-KTY       PIC S9(4) USAGE COMP.                        
           10 KFZVERWEND-KNR       PIC S9(15)V USAGE COMP-3.                    
           10 KFZNAEHSPEZ-KTY      PIC S9(4) USAGE COMP.                        
           10 KFZNAEHSPEZ-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 KFZAKZART-KTY        PIC S9(4) USAGE COMP.                        
           10 KFZAKZART-KNR        PIC S9(15)V USAGE COMP-3.                    
           10 KFZSFANZKH           PIC S9(4) USAGE COMP.                        
           10 KFZSFANZVK           PIC S9(4) USAGE COMP.                        
           10 KFZTARIFAUSWAHL      PIC X(1).                                    
           10 KFZAUFBAUART-KTY     PIC S9(4) USAGE COMP.                        
           10 KFZAUFBAUART-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 KFZHERSTELLTEXT      PIC X(25).                                   
           10 KFZALTERSGRP-KTY     PIC S9(4) USAGE COMP.                        
           10 KFZALTERSGRP-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 KFZLEASING           PIC S9(4) USAGE COMP.                        
           10 KFZBEITRANPASSAM     PIC X(10).                                   
           10 KFZBERECHNETAM       PIC X(10).                                   
           10 KFZBEITRGESAMNEU     PIC S9(5)V9(2) USAGE COMP-3.                 
           10 KFZBEITRGESAMALT     PIC S9(5)V9(2) USAGE COMP-3.                 
           10 KFZKMSTANDAKT        PIC S9(7)V USAGE COMP-3.                     
           10 KFZDATKMSTANDAKT     PIC X(10).                                   
           10 KFZABWFAHRLSTJH      PIC S9(7)V USAGE COMP-3.                     
           10 KFZSFRFREIKH         PIC X(1).                                    
           10 KFZSFRFREIVK         PIC X(1).                                    
           10 KFZPLATZ             PIC S9(3)V USAGE COMP-3.                     
           10 KFZGESGEW            PIC S9(7)V USAGE COMP-3.                     
           10 KFZKW                PIC S9(4)V USAGE COMP-3.                     
           10 KFZNUTZ              PIC S9(7)V USAGE COMP-3.                     
           10 KFZHALTERSCHL        PIC S9(4) USAGE COMP.                        
           10 KFZHALTERTEXT        PIC X(10).                                   
           10 KFZBEGFAHRERSCHL     PIC S9(4) USAGE COMP.                        
           10 KFZBEGFAHRERTEXT     PIC X(10).                                   
           10 KFZPSFANZKH          PIC S9(4) USAGE COMP.                        
           10 KFZPSFANZVK          PIC S9(4) USAGE COMP.                        
           10 KFZPAKET-KTY         PIC S9(4) USAGE COMP.                        
           10 KFZPAKET-KNR         PIC S9(15)V USAGE COMP-3.                    
           10 KFZSCOREWERT         PIC S9(3)V USAGE COMP-3.                     
           10 KFZSCOREDATUM        PIC X(10).                                   
           10 KFZTELEMATIKID       PIC S9(14)V USAGE COMP-3.                    
           10 KFZFALTERMAX-KTY     PIC S9(4) USAGE COMP.                        
           10 KFZFALTERMAX-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 KFZ-E-FZG-KZ         PIC X(1).                                    
           10 KFZFIRMENNR          PIC X(6).                                    
           10 KFZGEBDAT-VN         PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
