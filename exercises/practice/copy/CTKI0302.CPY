      *****************************************************************         
CCC005*    EXEC SQL DECLARE BAUSPAREN TABLE                                     
CCC004*    ( BASVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      BASGUTHABPRODLFDNR             SMALLINT NOT NULL,                  
CCC004*      BASKONTOSTANDKZID              SMALLINT,                           
CCC004*      BASBERWERTUNGSZAHL             DECIMAL(8, 5),                      
CCC004*      BASDARLZSGEMTARIF              DECIMAL(5, 2),                      
CCC004*      BASDATKUMULWOHNBPR             DATE,                               
CCC004*      BASDATWOHNBAUPRAEM             DATE,                               
CCC004*      BASDATZUTEILAUSSI              DATE,                               
CCC004*      BASDISAGIOPROZENT              DECIMAL(5, 2),                      
CCC004*      BASENDESTEUERLFZ               DATE,                               
CCC004*      BASFEHLBETRMINANSP             DECIMAL(11, 2),                     
CCC004*      BASFEHLBETRMAWKZ               CHAR(1) NOT NULL,                   
CCC004*      BASKUMULWOHNBAUPR              DECIMAL(11, 2),                     
CCC004*      BASKUMWOHNBPRWKZ               CHAR(1) NOT NULL,                   
CCC004*      BASSTEUERBESCHEIKZ             CHAR(1) NOT NULL,                   
CCC004*      BASTARIFVARINTIII              CHAR(1) NOT NULL,                   
CCC004*      BASVERTRAGSSUMME               DECIMAL(11, 2),                     
CCC004*      BASVTSUMMEWKZ                  CHAR(1) NOT NULL,                   
CCC004*      BASMINDESTANSPROZ              DECIMAL(5, 2),                      
CCC004*      BASPRODSCHL                    CHAR(2),                            
CCC004*      BASBONUSVORJAHR                DECIMAL(11, 2),                     
CCC004*      BASBWZVORJAHR                  DECIMAL(8, 5),                      
CCC004*      BASZINSLFDJAHR                 DECIMAL(11, 2),                     
CCC004*      BASWOHNSPARKEY                 SMALLINT,                           
CCC004*      BASERHOEHEZBPROZ               DECIMAL(5, 2),                      
CCC004*      BASVERMITTELTKZ                CHAR(1),                            
CCC004*      BASBLANKODARLKZ                CHAR(1),                            
CCC004*      BASDATSPERRFR                  DATE,                               
CCC004*      BASZINSVORTRAG                 DECIMAL(11, 2),                     
CCC004*      BASDATAGEING                   DATE,                               
CCC004*      BASFINPRODSCHL                 SMALLINT,                           
CCC004*      BASDATZINSFESTAB               DATE,                               
CCC004*      BASDATZINSFESTBIS              DATE,                               
CCC004*      BASZINSNOMINAL                 DECIMAL(6, 3),                      
CCC004*      BASZINSEFFEKTIV                DECIMAL(6, 3),                      
CCC004*      BASANNURATE                    DECIMAL(11, 2),                     
CCC004*      BASLAUFZEITFIN                 SMALLINT,                           
CCC004*      BASSONDTILGJAHR                DECIMAL(11, 2),                     
CCC004*      BASDARLHOEHE                   DECIMAL(11, 2),                     
CCC004*      BASDARLSALDO                   DECIMAL(11, 2),                     
CCC004*      BASDATAFBEGINN                 DATE,                               
CCC004*      BASBONUSFAKTOR                 DECIMAL(4, 2),                      
CCC004*      BASBONUSAKTUELL                DECIMAL(11, 2),                     
CCC004*      BASZINSPLUSGENER               DECIMAL(2, 0),                      
CCC004*      BASBEITRAG                     DECIMAL(11, 2),                     
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      BASEINZBETRAGMON               DECIMAL(9, 2),                      
CCC004*      BASEINZGDATUMMON               DATE,                               
CCC004*      BASEINZBETRAGJHR               DECIMAL(9, 2),                      
CCC004*      BASEINZGDATUMJHR               DATE,                               
CCC004*      BASEINZUGDYNAM                 DECIMAL(5, 2),                      
CCC004*      BASMINBEWERTZAHL               DECIMAL(6, 3),                      
CCC004*      BASSTEUERLFZDAT                DATE,                               
CCC004*      BASWOPKZ                       CHAR(1),                            
CCC004*      BASDAUERZULAGEKZ               CHAR(1),                            
CCC004*      BASVARIANWECHSKZ               CHAR(1),                            
CCC004*      BASFINANZIERNGKZ               CHAR(1),                            
CCC004*      BASZUTEILANWDAT                DATE,                               
CCC004*      BASDARLEHNAKTEKZ               CHAR(1),                            
CCC004*      BASENDEBINDFRIST               DATE,                               
CCC004*      BASLETZTEDATAKTU               DATE,                               
CCC004*      BASZIELTILGUNG                 DECIMAL(5, 2),                      
CCC004*      BASMEHRZUTEILUNG               DECIMAL(9, 2),                      
CCC004*      BASABTRETUNGBLZ                INTEGER,                            
CCC004*      BASTARIFUMSTBTRG               DECIMAL(7, 2),                      
CCC004*      BASANFTILGUNG                  DECIMAL(5, 2),                      
CCC004*      BASLETZTEZUTEIL                DATE,                               
CCC004*      BASZUTEILANWART                DATE,                               
CCC004*      BASWOPANSPRUCH                 DECIMAL(9, 2),                      
CCC004*      BASSPARZULFREIAB               DATE,                               
CCC004*      BASVWLKZ                       CHAR(1),                            
CCC004*      BASIMMOFITKZ                   CHAR(1),                            
CCC004*      BASWERBVERBOTKZ                CHAR(1),                            
CCC004*      BASDATGHBZINSBEGR              DATE,                               
CCC004*      BASMAXZINSKAPITAL              DECIMAL(11, 2),                     
CCC004*      BASMIETKAUTIONKZ               CHAR(1),                            
CCC004*      BASPASSWVEREINB                CHAR(1),                            
CCC004*      BASDATLETZTERKAZ               DATE,                               
CCC004*      BASNRLETZTERKAZ                CHAR(7),                            
CCC004*      BASDATNVBESCHEIN               DATE,                               
CCC004*      BASVLPERSONALTKZ               CHAR(1),                            
CCC004*      BASONLKIPROD_KTY               SMALLINT,                           
CCC004*      BASONLKIPROD_KNR               DECIMAL(15, 0),                     
CCC004*      BASONLKIART_KTY                SMALLINT,                           
CCC004*      BASONLKIART_KNR                DECIMAL(15, 0),                     
CCC004*      BASFRONTTEC_KTY                SMALLINT,                           
CCC004*      BASFRONTTEC_KNR                DECIMAL(15, 0),                     
CCC004*      ABB_VERSION                    CHAR(7),                            
CCC004*      BASPRODUKTID                   SMALLINT NOT NULL,                  
CCC004*      BASSCHLIESSDATUM               DATE,                               
CCC004*      BASZINSSATZ                    DECIMAL(5, 2),                      
CCC004*      BASGUTHABEN                    DECIMAL(11, 2),                     
CCC004*      BASBONUSART                    CHAR(2),                            
CCC004*      BASBONUSBEGDATUM               DATE,                               
CCC004*      BASTUBAKTION                   CHAR(1),                            
CCC004*      BASSONDERZAHLUNG               DECIMAL(5, 2),                      
CCC004*      BASSONDERTILGUNG               DECIMAL(5, 2),                      
CCC004*      BASPEAUSZAHLART                CHAR(1)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE BAUSPAREN                   *               
      ******************************************************************        
       01  BAUSPAREN-GESAMT.                                                    
        05 NI-BAUSPAREN-TAB.                                                    
           10 NI-BAUSPAREN-ELE PIC S9(04) COMP OCCURS 98 TIMES.                 
        05 NI-BAUSPAREN REDEFINES NI-BAUSPAREN-TAB.                             
           10 NI-BASVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-BASGUTHABPRODLFDNR PIC S9(4) COMP.                             
           10 NI-BASKONTOSTANDKZID  PIC S9(4) COMP.                             
           10 NI-BASBERWERTUNGSZAHL PIC S9(4) COMP.                             
           10 NI-BASDARLZSGEMTARIF  PIC S9(4) COMP.                             
           10 NI-BASDATKUMULWOHNBPR PIC S9(4) COMP.                             
           10 NI-BASDATWOHNBAUPRAEM PIC S9(4) COMP.                             
           10 NI-BASDATZUTEILAUSSI  PIC S9(4) COMP.                             
           10 NI-BASDISAGIOPROZENT  PIC S9(4) COMP.                             
           10 NI-BASENDESTEUERLFZ   PIC S9(4) COMP.                             
           10 NI-BASFEHLBETRMINANSP PIC S9(4) COMP.                             
           10 NI-BASFEHLBETRMAWKZ   PIC S9(4) COMP.                             
           10 NI-BASKUMULWOHNBAUPR  PIC S9(4) COMP.                             
           10 NI-BASKUMWOHNBPRWKZ   PIC S9(4) COMP.                             
           10 NI-BASSTEUERBESCHEIKZ PIC S9(4) COMP.                             
           10 NI-BASTARIFVARINTIII  PIC S9(4) COMP.                             
           10 NI-BASVERTRAGSSUMME   PIC S9(4) COMP.                             
           10 NI-BASVTSUMMEWKZ      PIC S9(4) COMP.                             
           10 NI-BASMINDESTANSPROZ  PIC S9(4) COMP.                             
           10 NI-BASPRODSCHL        PIC S9(4) COMP.                             
           10 NI-BASBONUSVORJAHR    PIC S9(4) COMP.                             
           10 NI-BASBWZVORJAHR      PIC S9(4) COMP.                             
           10 NI-BASZINSLFDJAHR     PIC S9(4) COMP.                             
           10 NI-BASWOHNSPARKEY     PIC S9(4) COMP.                             
           10 NI-BASERHOEHEZBPROZ   PIC S9(4) COMP.                             
           10 NI-BASVERMITTELTKZ    PIC S9(4) COMP.                             
           10 NI-BASBLANKODARLKZ    PIC S9(4) COMP.                             
           10 NI-BASDATSPERRFR      PIC S9(4) COMP.                             
           10 NI-BASZINSVORTRAG     PIC S9(4) COMP.                             
           10 NI-BASDATAGEING       PIC S9(4) COMP.                             
           10 NI-BASFINPRODSCHL     PIC S9(4) COMP.                             
           10 NI-BASDATZINSFESTAB   PIC S9(4) COMP.                             
           10 NI-BASDATZINSFESTBIS  PIC S9(4) COMP.                             
           10 NI-BASZINSNOMINAL     PIC S9(4) COMP.                             
           10 NI-BASZINSEFFEKTIV    PIC S9(4) COMP.                             
           10 NI-BASANNURATE        PIC S9(4) COMP.                             
           10 NI-BASLAUFZEITFIN     PIC S9(4) COMP.                             
           10 NI-BASSONDTILGJAHR    PIC S9(4) COMP.                             
           10 NI-BASDARLHOEHE       PIC S9(4) COMP.                             
           10 NI-BASDARLSALDO       PIC S9(4) COMP.                             
           10 NI-BASDATAFBEGINN     PIC S9(4) COMP.                             
           10 NI-BASBONUSFAKTOR     PIC S9(4) COMP.                             
           10 NI-BASBONUSAKTUELL    PIC S9(4) COMP.                             
           10 NI-BASZINSPLUSGENER   PIC S9(4) COMP.                             
           10 NI-BASBEITRAG         PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-BASEINZBETRAGMON   PIC S9(4) COMP.                             
           10 NI-BASEINZGDATUMMON   PIC S9(4) COMP.                             
           10 NI-BASEINZBETRAGJHR   PIC S9(4) COMP.                             
           10 NI-BASEINZGDATUMJHR   PIC S9(4) COMP.                             
           10 NI-BASEINZUGDYNAM     PIC S9(4) COMP.                             
           10 NI-BASMINBEWERTZAHL   PIC S9(4) COMP.                             
           10 NI-BASSTEUERLFZDAT    PIC S9(4) COMP.                             
           10 NI-BASWOPKZ           PIC S9(4) COMP.                             
           10 NI-BASDAUERZULAGEKZ   PIC S9(4) COMP.                             
           10 NI-BASVARIANWECHSKZ   PIC S9(4) COMP.                             
           10 NI-BASFINANZIERNGKZ   PIC S9(4) COMP.                             
           10 NI-BASZUTEILANWDAT    PIC S9(4) COMP.                             
           10 NI-BASDARLEHNAKTEKZ   PIC S9(4) COMP.                             
           10 NI-BASENDEBINDFRIST   PIC S9(4) COMP.                             
           10 NI-BASLETZTEDATAKTU   PIC S9(4) COMP.                             
           10 NI-BASZIELTILGUNG     PIC S9(4) COMP.                             
           10 NI-BASMEHRZUTEILUNG   PIC S9(4) COMP.                             
           10 NI-BASABTRETUNGBLZ    PIC S9(4) COMP.                             
           10 NI-BASTARIFUMSTBTRG   PIC S9(4) COMP.                             
           10 NI-BASANFTILGUNG      PIC S9(4) COMP.                             
           10 NI-BASLETZTEZUTEIL    PIC S9(4) COMP.                             
           10 NI-BASZUTEILANWART    PIC S9(4) COMP.                             
           10 NI-BASWOPANSPRUCH     PIC S9(4) COMP.                             
           10 NI-BASSPARZULFREIAB   PIC S9(4) COMP.                             
           10 NI-BASVWLKZ           PIC S9(4) COMP.                             
           10 NI-BASIMMOFITKZ       PIC S9(4) COMP.                             
           10 NI-BASWERBVERBOTKZ    PIC S9(4) COMP.                             
           10 NI-BASDATGHBZINSBEGR  PIC S9(4) COMP.                             
           10 NI-BASMAXZINSKAPITAL  PIC S9(4) COMP.                             
           10 NI-BASMIETKAUTIONKZ   PIC S9(4) COMP.                             
           10 NI-BASPASSWVEREINB    PIC S9(4) COMP.                             
           10 NI-BASDATLETZTERKAZ   PIC S9(4) COMP.                             
           10 NI-BASNRLETZTERKAZ    PIC S9(4) COMP.                             
           10 NI-BASDATNVBESCHEIN   PIC S9(4) COMP.                             
           10 NI-BASVLPERSONALTKZ   PIC S9(4) COMP.                             
           10 NI-BASONLKIPROD-KTY   PIC S9(4) COMP.                             
           10 NI-BASONLKIPROD-KNR   PIC S9(4) COMP.                             
           10 NI-BASONLKIART-KTY    PIC S9(4) COMP.                             
           10 NI-BASONLKIART-KNR    PIC S9(4) COMP.                             
           10 NI-BASFRONTTEC-KTY    PIC S9(4) COMP.                             
           10 NI-BASFRONTTEC-KNR    PIC S9(4) COMP.                             
           10 NI-ABB-VERSION        PIC S9(4) COMP.                             
           10 NI-BASPRODUKTID       PIC S9(4) COMP.                             
           10 NI-BASSCHLIESSDATUM   PIC S9(4) COMP.                             
           10 NI-BASZINSSATZ        PIC S9(4) COMP.                             
           10 NI-BASGUTHABEN        PIC S9(4) COMP.                             
           10 NI-BASBONUSART        PIC S9(4) COMP.                             
           10 NI-BASBONUSBEGDATUM   PIC S9(4) COMP.                             
           10 NI-BASTUBAKTION       PIC S9(4) COMP.                             
           10 NI-BASSONDERZAHLUNG   PIC S9(4) COMP.                             
           10 NI-BASSONDERTILGUNG   PIC S9(4) COMP.                             
           10 NI-BASPEAUSZAHLART    PIC S9(4) COMP.                             
        05 DCL-BAUSPAREN.                                                       
           10 BASVERTRAGSID        PIC X(13).                                   
           10 BASGUTHABPRODLFDNR   PIC S9(4) USAGE COMP.                        
           10 BASKONTOSTANDKZID    PIC S9(4) USAGE COMP.                        
           10 BASBERWERTUNGSZAHL   PIC S9(3)V9(5) USAGE COMP-3.                 
           10 BASDARLZSGEMTARIF    PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASDATKUMULWOHNBPR   PIC X(10).                                   
           10 BASDATWOHNBAUPRAEM   PIC X(10).                                   
           10 BASDATZUTEILAUSSI    PIC X(10).                                   
           10 BASDISAGIOPROZENT    PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASENDESTEUERLFZ     PIC X(10).                                   
           10 BASFEHLBETRMINANSP   PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASFEHLBETRMAWKZ     PIC X(1).                                    
           10 BASKUMULWOHNBAUPR    PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASKUMWOHNBPRWKZ     PIC X(1).                                    
           10 BASSTEUERBESCHEIKZ   PIC X(1).                                    
           10 BASTARIFVARINTIII    PIC X(1).                                    
           10 BASVERTRAGSSUMME     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASVTSUMMEWKZ        PIC X(1).                                    
           10 BASMINDESTANSPROZ    PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASPRODSCHL          PIC X(2).                                    
           10 BASBONUSVORJAHR      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASBWZVORJAHR        PIC S9(3)V9(5) USAGE COMP-3.                 
           10 BASZINSLFDJAHR       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASWOHNSPARKEY       PIC S9(4) USAGE COMP.                        
           10 BASERHOEHEZBPROZ     PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASVERMITTELTKZ      PIC X(1).                                    
           10 BASBLANKODARLKZ      PIC X(1).                                    
           10 BASDATSPERRFR        PIC X(10).                                   
           10 BASZINSVORTRAG       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASDATAGEING         PIC X(10).                                   
           10 BASFINPRODSCHL       PIC S9(4) USAGE COMP.                        
           10 BASDATZINSFESTAB     PIC X(10).                                   
           10 BASDATZINSFESTBIS    PIC X(10).                                   
           10 BASZINSNOMINAL       PIC S9(3)V9(3) USAGE COMP-3.                 
           10 BASZINSEFFEKTIV      PIC S9(3)V9(3) USAGE COMP-3.                 
           10 BASANNURATE          PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASLAUFZEITFIN       PIC S9(4) USAGE COMP.                        
           10 BASSONDTILGJAHR      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASDARLHOEHE         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASDARLSALDO         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASDATAFBEGINN       PIC X(10).                                   
           10 BASBONUSFAKTOR       PIC S9(2)V9(2) USAGE COMP-3.                 
           10 BASBONUSAKTUELL      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASZINSPLUSGENER     PIC S9(2)V USAGE COMP-3.                     
           10 BASBEITRAG           PIC S9(9)V9(2) USAGE COMP-3.                 
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 BASEINZBETRAGMON     PIC S9(7)V9(2) USAGE COMP-3.                 
           10 BASEINZGDATUMMON     PIC X(10).                                   
           10 BASEINZBETRAGJHR     PIC S9(7)V9(2) USAGE COMP-3.                 
           10 BASEINZGDATUMJHR     PIC X(10).                                   
           10 BASEINZUGDYNAM       PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASMINBEWERTZAHL     PIC S9(3)V9(3) USAGE COMP-3.                 
           10 BASSTEUERLFZDAT      PIC X(10).                                   
           10 BASWOPKZ             PIC X(1).                                    
           10 BASDAUERZULAGEKZ     PIC X(1).                                    
           10 BASVARIANWECHSKZ     PIC X(1).                                    
           10 BASFINANZIERNGKZ     PIC X(1).                                    
           10 BASZUTEILANWDAT      PIC X(10).                                   
           10 BASDARLEHNAKTEKZ     PIC X(1).                                    
           10 BASENDEBINDFRIST     PIC X(10).                                   
           10 BASLETZTEDATAKTU     PIC X(10).                                   
           10 BASZIELTILGUNG       PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASMEHRZUTEILUNG     PIC S9(7)V9(2) USAGE COMP-3.                 
           10 BASABTRETUNGBLZ      PIC S9(9) USAGE COMP.                        
           10 BASTARIFUMSTBTRG     PIC S9(5)V9(2) USAGE COMP-3.                 
           10 BASANFTILGUNG        PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASLETZTEZUTEIL      PIC X(10).                                   
           10 BASZUTEILANWART      PIC X(10).                                   
           10 BASWOPANSPRUCH       PIC S9(7)V9(2) USAGE COMP-3.                 
           10 BASSPARZULFREIAB     PIC X(10).                                   
           10 BASVWLKZ             PIC X(1).                                    
           10 BASIMMOFITKZ         PIC X(1).                                    
           10 BASWERBVERBOTKZ      PIC X(1).                                    
           10 BASDATGHBZINSBEGR    PIC X(10).                                   
           10 BASMAXZINSKAPITAL    PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASMIETKAUTIONKZ     PIC X(1).                                    
           10 BASPASSWVEREINB      PIC X(1).                                    
           10 BASDATLETZTERKAZ     PIC X(10).                                   
           10 BASNRLETZTERKAZ      PIC X(7).                                    
           10 BASDATNVBESCHEIN     PIC X(10).                                   
           10 BASVLPERSONALTKZ     PIC X(1).                                    
           10 BASONLKIPROD-KTY     PIC S9(4) USAGE COMP.                        
           10 BASONLKIPROD-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 BASONLKIART-KTY      PIC S9(4) USAGE COMP.                        
           10 BASONLKIART-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 BASFRONTTEC-KTY      PIC S9(4) USAGE COMP.                        
           10 BASFRONTTEC-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 ABB-VERSION          PIC X(7).                                    
           10 BASPRODUKTID         PIC S9(4) USAGE COMP.                        
           10 BASSCHLIESSDATUM     PIC X(10).                                   
           10 BASZINSSATZ          PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASGUTHABEN          PIC S9(9)V9(2) USAGE COMP-3.                 
           10 BASBONUSART          PIC X(2).                                    
           10 BASBONUSBEGDATUM     PIC X(10).                                   
           10 BASTUBAKTION         PIC X(1).                                    
           10 BASSONDERZAHLUNG     PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASSONDERTILGUNG     PIC S9(3)V9(2) USAGE COMP-3.                 
           10 BASPEAUSZAHLART      PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
