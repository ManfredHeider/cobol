      *****************************************************************         
CCC005*    EXEC SQL DECLARE ZIELMARKT TABLE                                     
CCC004*    ( ZMAFONDSID                     SMALLINT NOT NULL,                  
CCC004*      ZMAWKN                         CHAR(6) NOT NULL,                   
CCC004*      ZMAISIN                        CHAR(12) NOT NULL,                  
CCC004*      ZMAFONDSKOSTEN                 DECIMAL(6, 2),                      
CCC004*      ZMAFONDSKOSTDAT                DATE,                               
CCC004*      ZMASCHAETZFOKO                 CHAR(1),                            
CCC004*      ZMATRANSKOSTEN                 DECIMAL(6, 2),                      
CCC004*      ZMATRANSKOSTDAT                DATE,                               
CCC004*      ZMASCHAETZTRKO                 CHAR(1),                            
CCC004*      ZMAPERFFEE                     DECIMAL(6, 2),                      
CCC004*      ZMAPERFFEEDAT                  DATE,                               
CCC004*      ZMASCHAETZPEFE                 CHAR(1),                            
CCC004*      ZMARUEKOST                     DECIMAL(6, 2),                      
CCC004*      ZMARUEKOSTDAT                  DATE,                               
CCC004*      ZMASCHAETZRUEKO                CHAR(1),                            
CCC004*      ZMAAUSGABEMIN                  DECIMAL(14, 8),                     
CCC004*      ZMAAUSGABEMAX                  DECIMAL(14, 8),                     
CCC004*      ZMARUECKMIN                    DECIMAL(14, 8),                     
CCC004*      ZMARUECKMAX                    DECIMAL(14, 8),                     
CCC004*      ZMASCHAETZRUEAB                CHAR(1),                            
CCC004*      ZMADISKUNKAT_KTY               SMALLINT NOT NULL,                  
CCC004*      ZMADISKUNKAT_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADISANLZIEL_KTY              SMALLINT NOT NULL,                  
CCC004*      ZMADISANLZIEL_KNR              DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADISANLHORIZ_KTY             SMALLINT NOT NULL,                  
CCC004*      ZMADISANLHORIZ_KNR             DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADISKENNTNIS_KTY             SMALLINT NOT NULL,                  
CCC004*      ZMADISKENNTNIS_KNR             DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADISVERLTF_KTY               SMALLINT NOT NULL,                  
CCC004*      ZMADISVERLTF_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADISSTRAT_KTY                SMALLINT NOT NULL,                  
CCC004*      ZMADISSTRAT_KNR                DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAANLINFO                     CHAR(1),                            
CCC004*      ZMAANLINFOEDH                  CHAR(1),                            
CCC004*      ZMAHEBELKZ                     CHAR(1),                            
CCC004*      ZMATICKGR                      CHAR(1),                            
CCC004*      ZMAMINDESTANL                  DECIMAL(18, 3),                     
CCC004*      ZMAFONDSKAT                    CHAR(1),                            
CCC004*      ZMAVLFONDS                     CHAR(1),                            
CCC004*      ZMAAIGKZ                       CHAR(1),                            
CCC004*      ZMAASFKAT                      CHAR(1),                            
CCC004*      ZMAASFTYP                      CHAR(1),                            
CCC004*      ZMAASFSONDER                   CHAR(1),                            
CCC004*      ZMAASFGARANT_KTY               SMALLINT NOT NULL,                  
CCC004*      ZMAASFGARANT_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAASFLAUFZ_KTY                SMALLINT NOT NULL,                  
CCC004*      ZMAASFLAUFZ_KNR                DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAASFLAND                     CHAR(2),                            
CCC004*      ZMAASFWAEHRG                   CHAR(2),                            
CCC004*      ZMAASFBRANCHE                  CHAR(2),                            
CCC004*      ZMAAVWICKLG                    CHAR(1),                            
CCC004*      ZMAVERTRZULDE                  CHAR(2),                            
CCC004*      ZMAVERTRZULAT                  CHAR(2),                            
CCC004*      ZMARISIKOKL                    CHAR(1),                            
CCC004*      ZMAEMIKUNKAT_KTY               SMALLINT NOT NULL,                  
CCC004*      ZMAEMIKUNKAT_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMIANLZIEL_KTY              SMALLINT NOT NULL,                  
CCC004*      ZMAEMIANLZIEL_KNR              DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMIANLHORIZ_KTY             SMALLINT NOT NULL,                  
CCC004*      ZMAEMIANLHORIZ_KNR             DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMIKENNTNIS_KTY             SMALLINT NOT NULL,                  
CCC004*      ZMAEMIKENNTNIS_KNR             DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMIVERLTF_KTY               SMALLINT NOT NULL,                  
CCC004*      ZMAEMIVERLTF_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMIRISIKOPROF               CHAR(1),                            
CCC004*      ZMAEMIVERSTRAT_KTY             SMALLINT NOT NULL,                  
CCC004*      ZMAEMIVERSTRAT_KNR             DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMAEMISPEZANF_KTY              SMALLINT NOT NULL,                  
CCC004*      ZMAEMISPEZANF_KNR              DECIMAL(15, 0) NOT NULL,            
CCC004*      ZMADVAGFONDS                   CHAR(1),                            
CCC004*      ZMADVAGWIENFONDS               CHAR(1),                            
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      ZMAGENFONDS                    CHAR(1),                            
CCC004*      ZMADEVABRECHBRIEF              DECIMAL(12, 4) NOT NULL,            
CCC004*      ZMADEVABRECHGELD               DECIMAL(12, 4) NOT NULL,            
CCC004*      ZMADEVMITTELKURS               DECIMAL(13, 5) NOT NULL,            
CCC004*      ZMAWPGRUPPE                    CHAR(2) NOT NULL,                   
CCC004*      ZMAISTINVEST                   CHAR(1) NOT NULL,                   
CCC004*      ZMAETFETC                      CHAR(4),                            
CCC004*      ZMAFONDSART                    CHAR(20),                           
CCC004*      ZMAGESAUSAUF                   DECIMAL(9, 6) NOT NULL,             
CCC004*      ZMAGESAUSAUFKN                 CHAR(1) NOT NULL,                   
CCC004*      ZMAESGPRAEFERENZ               CHAR(1),                            
CCC004*      ZMAESGSCHWERPUNKT              CHAR(1),                            
CCC004*      ZMAESGINV                      DECIMAL(9, 4),                      
CCC004*      ZMAESGINVDAT                   DATE,                               
CCC004*      ZMAESGLETZTINV                 DECIMAL(9, 4),                      
CCC004*      ZMAESGLETZTINVDAT              DATE,                               
CCC004*      ZMAESGANTOEKO                  DECIMAL(9, 4),                      
CCC004*      ZMAESGANTOEKODAT               DATE,                               
CCC004*      ZMAESGLZTATOEKO                DECIMAL(9, 4),                      
CCC004*      ZMAESGLZTATOEKODAT             DATE,                               
CCC004*      ZMAESGUZ01                     CHAR(1),                            
CCC004*      ZMAESGUZ02                     CHAR(1),                            
CCC004*      ZMAESGUZ03                     CHAR(1),                            
CCC004*      ZMAESGUZ04                     CHAR(1),                            
CCC004*      ZMAESGUZ05                     CHAR(1),                            
CCC004*      ZMAESGUZ06                     CHAR(1),                            
CCC004*      ZMAESGZUSINFOSFDR              CHAR(2)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE ZIELMARKT                   *               
      ******************************************************************        
       01  ZIELMARKT-GESAMT.                                                    
        05 NI-ZIELMARKT-TAB.                                                    
           10 NI-ZIELMARKT-ELE PIC S9(04) COMP OCCURS 99 TIMES.                 
        05 NI-ZIELMARKT REDEFINES NI-ZIELMARKT-TAB.                             
           10 NI-ZMAFONDSID         PIC S9(4) COMP.                             
           10 NI-ZMAWKN             PIC S9(4) COMP.                             
           10 NI-ZMAISIN            PIC S9(4) COMP.                             
           10 NI-ZMAFONDSKOSTEN     PIC S9(4) COMP.                             
           10 NI-ZMAFONDSKOSTDAT    PIC S9(4) COMP.                             
           10 NI-ZMASCHAETZFOKO     PIC S9(4) COMP.                             
           10 NI-ZMATRANSKOSTEN     PIC S9(4) COMP.                             
           10 NI-ZMATRANSKOSTDAT    PIC S9(4) COMP.                             
           10 NI-ZMASCHAETZTRKO     PIC S9(4) COMP.                             
           10 NI-ZMAPERFFEE         PIC S9(4) COMP.                             
           10 NI-ZMAPERFFEEDAT      PIC S9(4) COMP.                             
           10 NI-ZMASCHAETZPEFE     PIC S9(4) COMP.                             
           10 NI-ZMARUEKOST         PIC S9(4) COMP.                             
           10 NI-ZMARUEKOSTDAT      PIC S9(4) COMP.                             
           10 NI-ZMASCHAETZRUEKO    PIC S9(4) COMP.                             
           10 NI-ZMAAUSGABEMIN      PIC S9(4) COMP.                             
           10 NI-ZMAAUSGABEMAX      PIC S9(4) COMP.                             
           10 NI-ZMARUECKMIN        PIC S9(4) COMP.                             
           10 NI-ZMARUECKMAX        PIC S9(4) COMP.                             
           10 NI-ZMASCHAETZRUEAB    PIC S9(4) COMP.                             
           10 NI-ZMADISKUNKAT-KTY   PIC S9(4) COMP.                             
           10 NI-ZMADISKUNKAT-KNR   PIC S9(4) COMP.                             
           10 NI-ZMADISANLZIEL-KTY  PIC S9(4) COMP.                             
           10 NI-ZMADISANLZIEL-KNR  PIC S9(4) COMP.                             
           10 NI-ZMADISANLHORIZ-KTY PIC S9(4) COMP.                             
           10 NI-ZMADISANLHORIZ-KNR PIC S9(4) COMP.                             
           10 NI-ZMADISKENNTNIS-KTY PIC S9(4) COMP.                             
           10 NI-ZMADISKENNTNIS-KNR PIC S9(4) COMP.                             
           10 NI-ZMADISVERLTF-KTY   PIC S9(4) COMP.                             
           10 NI-ZMADISVERLTF-KNR   PIC S9(4) COMP.                             
           10 NI-ZMADISSTRAT-KTY    PIC S9(4) COMP.                             
           10 NI-ZMADISSTRAT-KNR    PIC S9(4) COMP.                             
           10 NI-ZMAANLINFO         PIC S9(4) COMP.                             
           10 NI-ZMAANLINFOEDH      PIC S9(4) COMP.                             
           10 NI-ZMAHEBELKZ         PIC S9(4) COMP.                             
           10 NI-ZMATICKGR          PIC S9(4) COMP.                             
           10 NI-ZMAMINDESTANL      PIC S9(4) COMP.                             
           10 NI-ZMAFONDSKAT        PIC S9(4) COMP.                             
           10 NI-ZMAVLFONDS         PIC S9(4) COMP.                             
           10 NI-ZMAAIGKZ           PIC S9(4) COMP.                             
           10 NI-ZMAASFKAT          PIC S9(4) COMP.                             
           10 NI-ZMAASFTYP          PIC S9(4) COMP.                             
           10 NI-ZMAASFSONDER       PIC S9(4) COMP.                             
           10 NI-ZMAASFGARANT-KTY   PIC S9(4) COMP.                             
           10 NI-ZMAASFGARANT-KNR   PIC S9(4) COMP.                             
           10 NI-ZMAASFLAUFZ-KTY    PIC S9(4) COMP.                             
           10 NI-ZMAASFLAUFZ-KNR    PIC S9(4) COMP.                             
           10 NI-ZMAASFLAND         PIC S9(4) COMP.                             
           10 NI-ZMAASFWAEHRG       PIC S9(4) COMP.                             
           10 NI-ZMAASFBRANCHE      PIC S9(4) COMP.                             
           10 NI-ZMAAVWICKLG        PIC S9(4) COMP.                             
           10 NI-ZMAVERTRZULDE      PIC S9(4) COMP.                             
           10 NI-ZMAVERTRZULAT      PIC S9(4) COMP.                             
           10 NI-ZMARISIKOKL        PIC S9(4) COMP.                             
           10 NI-ZMAEMIKUNKAT-KTY   PIC S9(4) COMP.                             
           10 NI-ZMAEMIKUNKAT-KNR   PIC S9(4) COMP.                             
           10 NI-ZMAEMIANLZIEL-KTY  PIC S9(4) COMP.                             
           10 NI-ZMAEMIANLZIEL-KNR  PIC S9(4) COMP.                             
           10 NI-ZMAEMIANLHORIZ-KTY PIC S9(4) COMP.                             
           10 NI-ZMAEMIANLHORIZ-KNR PIC S9(4) COMP.                             
           10 NI-ZMAEMIKENNTNIS-KTY PIC S9(4) COMP.                             
           10 NI-ZMAEMIKENNTNIS-KNR PIC S9(4) COMP.                             
           10 NI-ZMAEMIVERLTF-KTY   PIC S9(4) COMP.                             
           10 NI-ZMAEMIVERLTF-KNR   PIC S9(4) COMP.                             
           10 NI-ZMAEMIRISIKOPROF   PIC S9(4) COMP.                             
           10 NI-ZMAEMIVERSTRAT-KTY PIC S9(4) COMP.                             
           10 NI-ZMAEMIVERSTRAT-KNR PIC S9(4) COMP.                             
           10 NI-ZMAEMISPEZANF-KTY  PIC S9(4) COMP.                             
           10 NI-ZMAEMISPEZANF-KNR  PIC S9(4) COMP.                             
           10 NI-ZMADVAGFONDS       PIC S9(4) COMP.                             
           10 NI-ZMADVAGWIENFONDS   PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-ZMAGENFONDS        PIC S9(4) COMP.                             
           10 NI-ZMADEVABRECHBRIEF  PIC S9(4) COMP.                             
           10 NI-ZMADEVABRECHGELD   PIC S9(4) COMP.                             
           10 NI-ZMADEVMITTELKURS   PIC S9(4) COMP.                             
           10 NI-ZMAWPGRUPPE        PIC S9(4) COMP.                             
           10 NI-ZMAISTINVEST       PIC S9(4) COMP.                             
           10 NI-ZMAETFETC          PIC S9(4) COMP.                             
           10 NI-ZMAFONDSART        PIC S9(4) COMP.                             
           10 NI-ZMAGESAUSAUF       PIC S9(4) COMP.                             
           10 NI-ZMAGESAUSAUFKN     PIC S9(4) COMP.                             
           10 NI-ZMAESGPRAEFERENZ   PIC S9(4) COMP.                             
           10 NI-ZMAESGSCHWERPUNKT  PIC S9(4) COMP.                             
           10 NI-ZMAESGINV          PIC S9(4) COMP.                             
           10 NI-ZMAESGINVDAT       PIC S9(4) COMP.                             
           10 NI-ZMAESGLETZTINV     PIC S9(4) COMP.                             
           10 NI-ZMAESGLETZTINVDAT  PIC S9(4) COMP.                             
           10 NI-ZMAESGANTOEKO      PIC S9(4) COMP.                             
           10 NI-ZMAESGANTOEKODAT   PIC S9(4) COMP.                             
           10 NI-ZMAESGLZTATOEKO    PIC S9(4) COMP.                             
           10 NI-ZMAESGLZTATOEKODAT PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ01         PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ02         PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ03         PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ04         PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ05         PIC S9(4) COMP.                             
           10 NI-ZMAESGUZ06         PIC S9(4) COMP.                             
           10 NI-ZMAESGZUSINFOSFDR  PIC S9(4) COMP.                             
        05 DCL-ZIELMARKT.                                                       
           10 ZMAFONDSID           PIC S9(4) USAGE COMP-5.                      
           10 ZMAWKN               PIC X(6).                                    
           10 ZMAISIN              PIC X(12).                                   
           10 ZMAFONDSKOSTEN       PIC S9(4)V9(2) USAGE COMP-3.                 
           10 ZMAFONDSKOSTDAT      PIC X(10).                                   
           10 ZMASCHAETZFOKO       PIC X(1).                                    
           10 ZMATRANSKOSTEN       PIC S9(4)V9(2) USAGE COMP-3.                 
           10 ZMATRANSKOSTDAT      PIC X(10).                                   
           10 ZMASCHAETZTRKO       PIC X(1).                                    
           10 ZMAPERFFEE           PIC S9(4)V9(2) USAGE COMP-3.                 
           10 ZMAPERFFEEDAT        PIC X(10).                                   
           10 ZMASCHAETZPEFE       PIC X(1).                                    
           10 ZMARUEKOST           PIC S9(4)V9(2) USAGE COMP-3.                 
           10 ZMARUEKOSTDAT        PIC X(10).                                   
           10 ZMASCHAETZRUEKO      PIC X(1).                                    
           10 ZMAAUSGABEMIN        PIC S9(6)V9(8) USAGE COMP-3.                 
           10 ZMAAUSGABEMAX        PIC S9(6)V9(8) USAGE COMP-3.                 
           10 ZMARUECKMIN          PIC S9(6)V9(8) USAGE COMP-3.                 
           10 ZMARUECKMAX          PIC S9(6)V9(8) USAGE COMP-3.                 
           10 ZMASCHAETZRUEAB      PIC X(1).                                    
           10 ZMADISKUNKAT-KTY     PIC S9(4) USAGE COMP-5.                      
           10 ZMADISKUNKAT-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 ZMADISANLZIEL-KTY    PIC S9(4) USAGE COMP-5.                      
           10 ZMADISANLZIEL-KNR    PIC S9(15)V USAGE COMP-3.                    
           10 ZMADISANLHORIZ-KTY   PIC S9(4) USAGE COMP-5.                      
           10 ZMADISANLHORIZ-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 ZMADISKENNTNIS-KTY   PIC S9(4) USAGE COMP-5.                      
           10 ZMADISKENNTNIS-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 ZMADISVERLTF-KTY     PIC S9(4) USAGE COMP-5.                      
           10 ZMADISVERLTF-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 ZMADISSTRAT-KTY      PIC S9(4) USAGE COMP-5.                      
           10 ZMADISSTRAT-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 ZMAANLINFO           PIC X(1).                                    
           10 ZMAANLINFOEDH        PIC X(1).                                    
           10 ZMAHEBELKZ           PIC X(1).                                    
           10 ZMATICKGR            PIC X(1).                                    
           10 ZMAMINDESTANL        PIC S9(15)V9(3) USAGE COMP-3.                
           10 ZMAFONDSKAT          PIC X(1).                                    
           10 ZMAVLFONDS           PIC X(1).                                    
           10 ZMAAIGKZ             PIC X(1).                                    
           10 ZMAASFKAT            PIC X(1).                                    
           10 ZMAASFTYP            PIC X(1).                                    
           10 ZMAASFSONDER         PIC X(1).                                    
           10 ZMAASFGARANT-KTY     PIC S9(4) USAGE COMP-5.                      
           10 ZMAASFGARANT-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 ZMAASFLAUFZ-KTY      PIC S9(4) USAGE COMP-5.                      
           10 ZMAASFLAUFZ-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 ZMAASFLAND           PIC X(2).                                    
           10 ZMAASFWAEHRG         PIC X(2).                                    
           10 ZMAASFBRANCHE        PIC X(2).                                    
           10 ZMAAVWICKLG          PIC X(1).                                    
           10 ZMAVERTRZULDE        PIC X(2).                                    
           10 ZMAVERTRZULAT        PIC X(2).                                    
           10 ZMARISIKOKL          PIC X(1).                                    
           10 ZMAEMIKUNKAT-KTY     PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIKUNKAT-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMIANLZIEL-KTY    PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIANLZIEL-KNR    PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMIANLHORIZ-KTY   PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIANLHORIZ-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMIKENNTNIS-KTY   PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIKENNTNIS-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMIVERLTF-KTY     PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIVERLTF-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMIRISIKOPROF     PIC X(1).                                    
           10 ZMAEMIVERSTRAT-KTY   PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMIVERSTRAT-KNR   PIC S9(15)V USAGE COMP-3.                    
           10 ZMAEMISPEZANF-KTY    PIC S9(4) USAGE COMP-5.                      
           10 ZMAEMISPEZANF-KNR    PIC S9(15)V USAGE COMP-3.                    
           10 ZMADVAGFONDS         PIC X(1).                                    
           10 ZMADVAGWIENFONDS     PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 ZMAGENFONDS          PIC X(1).                                    
           10 ZMADEVABRECHBRIEF    PIC S9(8)V9(4) USAGE COMP-3.                 
           10 ZMADEVABRECHGELD     PIC S9(8)V9(4) USAGE COMP-3.                 
           10 ZMADEVMITTELKURS     PIC S9(8)V9(5) USAGE COMP-3.                 
           10 ZMAWPGRUPPE          PIC X(2).                                    
           10 ZMAISTINVEST         PIC X(1).                                    
           10 ZMAETFETC            PIC X(4).                                    
           10 ZMAFONDSART          PIC X(20).                                   
           10 ZMAGESAUSAUF         PIC S9(3)V9(6) USAGE COMP-3.                 
           10 ZMAGESAUSAUFKN       PIC X(1).                                    
           10 ZMAESGPRAEFERENZ     PIC X(1).                                    
           10 ZMAESGSCHWERPUNKT    PIC X(1).                                    
           10 ZMAESGINV            PIC S9(5)V9(4) USAGE COMP-3.                 
           10 ZMAESGINVDAT         PIC X(10).                                   
           10 ZMAESGLETZTINV       PIC S9(5)V9(4) USAGE COMP-3.                 
           10 ZMAESGLETZTINVDAT    PIC X(10).                                   
           10 ZMAESGANTOEKO        PIC S9(5)V9(4) USAGE COMP-3.                 
           10 ZMAESGANTOEKODAT     PIC X(10).                                   
           10 ZMAESGLZTATOEKO      PIC S9(5)V9(4) USAGE COMP-3.                 
           10 ZMAESGLZTATOEKODAT   PIC X(10).                                   
           10 ZMAESGUZ01           PIC X(1).                                    
           10 ZMAESGUZ02           PIC X(1).                                    
           10 ZMAESGUZ03           PIC X(1).                                    
           10 ZMAESGUZ04           PIC X(1).                                    
           10 ZMAESGUZ05           PIC X(1).                                    
           10 ZMAESGUZ06           PIC X(1).                                    
           10 ZMAESGZUSINFOSFDR    PIC X(2).                                    
      ************************** ENDE-DCLGEN *************************          
