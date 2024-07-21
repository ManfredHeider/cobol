      *****************************************************************         
CCC005*    EXEC SQL DECLARE GENVERTRAGDETAIL TABLE                              
CCC004*    ( GVDVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      GVDLFDNR                       SMALLINT NOT NULL,                  
CCC004*      GVDPRODUKTID                   SMALLINT NOT NULL,                  
CCC004*      GVDBEGINN                      DATE,                               
CCC004*      GVDABLAUF                      DATE,                               
CCC004*      GVDBEITRAG                     DECIMAL(10, 2) NOT NULL,            
CCC004*      GVDNJBEITRAG                   DECIMAL(10, 2),                     
CCC004*      GVDSPARTE                      CHAR(3) NOT NULL,                   
CCC004*      GVDPRODUKT                     CHAR(50),                           
CCC004*      GVDTARIF                       CHAR(50),                           
CCC004*      GVDTARPROD_KTY                 SMALLINT,                           
CCC004*      GVDTARPROD_KNR                 DECIMAL(15, 0),                     
CCC004*      GVDLVART                       SMALLINT,                           
CCC004*      GVDSUMME                       DECIMAL(10, 2),                     
CCC004*      GVDAMTKZ                       CHAR(12),                           
CCC004*      GVDWAGNISKZ                    SMALLINT,                           
CCC004*      GVDGUTHABEN                    DECIMAL(10, 2),                     
CCC004*      GVDAKTVWERT                    DECIMAL(10, 2),                     
CCC004*      GVDRENTENBEZUG                 CHAR(1),                            
CCC004*      GVDRENTEVORAUS                 DECIMAL(10, 2),                     
CCC004*      GVDRENTEGARANT                 DECIMAL(10, 2),                     
CCC004*      GVDBERECHDAT                   DATE,                               
CCC004*      GVDGESCHAEFTSZW                CHAR(2),                            
CCC004*      GVDFLAECHE                     INTEGER,                            
CCC004*      GVDPRODKOMBI                   INTEGER,                            
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      GVDMIG_KZN                     CHAR(1),                            
CCC004*      GVDRNTZAHLW_KTY                SMALLINT,                           
CCC004*      GVDRNTZAHLW_KNR                SMALLINT,                           
CCC004*      GVDLEISTUNG_KZN                CHAR(1),                            
CCC004*      GVDERLFALL_SUM                 DECIMAL(11, 2),                     
CCC004*      GVDTODFALL_SUM                 DECIMAL(11, 2),                     
CCC004*      GVDRNTFAKTOR                   DECIMAL(7, 2),                      
CCC004*      GVDKAPABFIND                   DECIMAL(11, 2),                     
CCC004*      GVDFONDSGUTH                   DECIMAL(11, 2)                      
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE GENVERTRAGDETAIL            *               
      ******************************************************************        
       01  GENVERTRAGDETAIL-GESAMT.                                             
        05 NI-GENVERTRAGDETAIL-TAB.                                             
           10 NI-GENVERTRAGDETAIL-ELE PIC S9(04) COMP OCCURS 35 TIMES.          
        05 NI-GENVERTRAGDETAIL REDEFINES NI-GENVERTRAGDETAIL-TAB.               
           10 NI-GVDVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-GVDLFDNR           PIC S9(4) COMP.                             
           10 NI-GVDPRODUKTID       PIC S9(4) COMP.                             
           10 NI-GVDBEGINN          PIC S9(4) COMP.                             
           10 NI-GVDABLAUF          PIC S9(4) COMP.                             
           10 NI-GVDBEITRAG         PIC S9(4) COMP.                             
           10 NI-GVDNJBEITRAG       PIC S9(4) COMP.                             
           10 NI-GVDSPARTE          PIC S9(4) COMP.                             
           10 NI-GVDPRODUKT         PIC S9(4) COMP.                             
           10 NI-GVDTARIF           PIC S9(4) COMP.                             
           10 NI-GVDTARPROD-KTY     PIC S9(4) COMP.                             
           10 NI-GVDTARPROD-KNR     PIC S9(4) COMP.                             
           10 NI-GVDLVART           PIC S9(4) COMP.                             
           10 NI-GVDSUMME           PIC S9(4) COMP.                             
           10 NI-GVDAMTKZ           PIC S9(4) COMP.                             
           10 NI-GVDWAGNISKZ        PIC S9(4) COMP.                             
           10 NI-GVDGUTHABEN        PIC S9(4) COMP.                             
           10 NI-GVDAKTVWERT        PIC S9(4) COMP.                             
           10 NI-GVDRENTENBEZUG     PIC S9(4) COMP.                             
           10 NI-GVDRENTEVORAUS     PIC S9(4) COMP.                             
           10 NI-GVDRENTEGARANT     PIC S9(4) COMP.                             
           10 NI-GVDBERECHDAT       PIC S9(4) COMP.                             
           10 NI-GVDGESCHAEFTSZW    PIC S9(4) COMP.                             
           10 NI-GVDFLAECHE         PIC S9(4) COMP.                             
           10 NI-GVDPRODKOMBI       PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-GVDMIG-KZN         PIC S9(4) COMP.                             
           10 NI-GVDRNTZAHLW-KTY    PIC S9(4) COMP.                             
           10 NI-GVDRNTZAHLW-KNR    PIC S9(4) COMP.                             
           10 NI-GVDLEISTUNG-KZN    PIC S9(4) COMP.                             
           10 NI-GVDERLFALL-SUM     PIC S9(4) COMP.                             
           10 NI-GVDTODFALL-SUM     PIC S9(4) COMP.                             
           10 NI-GVDRNTFAKTOR       PIC S9(4) COMP.                             
           10 NI-GVDKAPABFIND       PIC S9(4) COMP.                             
           10 NI-GVDFONDSGUTH       PIC S9(4) COMP.                             
        05 DCL-GENVERTRAGDETAIL.                                                
           10 GVDVERTRAGSID        PIC X(13).                                   
           10 GVDLFDNR             PIC S9(4) USAGE COMP-5.                      
           10 GVDPRODUKTID         PIC S9(4) USAGE COMP-5.                      
           10 GVDBEGINN            PIC X(10).                                   
           10 GVDABLAUF            PIC X(10).                                   
           10 GVDBEITRAG           PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDNJBEITRAG         PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDSPARTE            PIC X(3).                                    
           10 GVDPRODUKT           PIC X(50).                                   
           10 GVDTARIF             PIC X(50).                                   
           10 GVDTARPROD-KTY       PIC S9(4) USAGE COMP-5.                      
           10 GVDTARPROD-KNR       PIC S9(15)V USAGE COMP-3.                    
           10 GVDLVART             PIC S9(4) USAGE COMP-5.                      
           10 GVDSUMME             PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDAMTKZ             PIC X(12).                                   
           10 GVDWAGNISKZ          PIC S9(4) USAGE COMP-5.                      
           10 GVDGUTHABEN          PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDAKTVWERT          PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDRENTENBEZUG       PIC X(1).                                    
           10 GVDRENTEVORAUS       PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDRENTEGARANT       PIC S9(8)V9(2) USAGE COMP-3.                 
           10 GVDBERECHDAT         PIC X(10).                                   
           10 GVDGESCHAEFTSZW      PIC X(2).                                    
           10 GVDFLAECHE           PIC S9(9) USAGE COMP-5.                      
           10 GVDPRODKOMBI         PIC S9(9) USAGE COMP-5.                      
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 GVDMIG-KZN           PIC X(1).                                    
           10 GVDRNTZAHLW-KTY      PIC S9(4) USAGE COMP-5.                      
           10 GVDRNTZAHLW-KNR      PIC S9(4) USAGE COMP-5.                      
           10 GVDLEISTUNG-KZN      PIC X(1).                                    
           10 GVDERLFALL-SUM       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GVDTODFALL-SUM       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GVDRNTFAKTOR         PIC S9(5)V9(2) USAGE COMP-3.                 
           10 GVDKAPABFIND         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 GVDFONDSGUTH         PIC S9(9)V9(2) USAGE COMP-3.                 
      ************************** ENDE-DCLGEN *************************          
