      *****************************************************************         
CCC005*    EXEC SQL DECLARE KRTARIFARTBEZ TABLE                                 
CCC004*    ( KRTVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      KRTVERSTARLFDNR                SMALLINT NOT NULL,                  
CCC004*      KRTTARIFART_KNR                SMALLINT NOT NULL,                  
CCC004*      KRTDATABGANGVERTR              DATE,                               
CCC004*      KRTDATZUGANGVERTR              DATE,                               
CCC004*      KRTMONATSBEITRAG               DECIMAL(11, 2),                     
CCC004*      KRTMONBEITRWKZ                 CHAR(1) NOT NULL,                   
CCC004*      KRTSELBSTBETEILIG              DECIMAL(11, 2),                     
CCC004*      KRTSELBSTBETWKZ                CHAR(1) NOT NULL,                   
CCC004*      KRTTARIFNAMEN                  CHAR(8) NOT NULL,                   
CCC004*      KRTRISIKOZUSCHLAG              CHAR(10) NOT NULL,                  
CCC004*      KRTVERSLEISTUNG                DECIMAL(11, 2),                     
CCC004*      KRTVERSLSTWKZ                  CHAR(1) NOT NULL,                   
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      KRTANWARTSCHAFT                CHAR(2),                            
CCC004*      KRTBE_REDUK_BTG                DECIMAL(11, 2) NOT NULL,            
CCC004*      KRT_ANZ_TAR_DUP                SMALLINT NOT NULL,                  
CCC004*      KRTTARIFART_KTY                SMALLINT NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE KRTARIFARTBEZ               *               
      ******************************************************************        
       01  KRTARIFARTBEZ-GESAMT.                                                
        05 NI-KRTARIFARTBEZ-TAB.                                                
           10 NI-KRTARIFARTBEZ-ELE PIC S9(04) COMP OCCURS 18 TIMES.             
        05 NI-KRTARIFARTBEZ REDEFINES NI-KRTARIFARTBEZ-TAB.                     
           10 NI-KRTVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-KRTVERSTARLFDNR    PIC S9(4) COMP.                             
           10 NI-KRTTARIFART-KNR    PIC S9(4) COMP.                             
           10 NI-KRTDATABGANGVERTR  PIC S9(4) COMP.                             
           10 NI-KRTDATZUGANGVERTR  PIC S9(4) COMP.                             
           10 NI-KRTMONATSBEITRAG   PIC S9(4) COMP.                             
           10 NI-KRTMONBEITRWKZ     PIC S9(4) COMP.                             
           10 NI-KRTSELBSTBETEILIG  PIC S9(4) COMP.                             
           10 NI-KRTSELBSTBETWKZ    PIC S9(4) COMP.                             
           10 NI-KRTTARIFNAMEN      PIC S9(4) COMP.                             
           10 NI-KRTRISIKOZUSCHLAG  PIC S9(4) COMP.                             
           10 NI-KRTVERSLEISTUNG    PIC S9(4) COMP.                             
           10 NI-KRTVERSLSTWKZ      PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-KRTANWARTSCHAFT    PIC S9(4) COMP.                             
           10 NI-KRTBE-REDUK-BTG    PIC S9(4) COMP.                             
           10 NI-KRT-ANZ-TAR-DUP    PIC S9(4) COMP.                             
           10 NI-KRTTARIFART-KTY    PIC S9(4) COMP.                             
        05 DCL-KRTARIFARTBEZ.                                                   
           10 KRTVERTRAGSID        PIC X(13).                                   
           10 KRTVERSTARLFDNR      PIC S9(4) USAGE COMP.                        
           10 KRTTARIFART-KNR      PIC S9(4) USAGE COMP.                        
           10 KRTDATABGANGVERTR    PIC X(10).                                   
           10 KRTDATZUGANGVERTR    PIC X(10).                                   
           10 KRTMONATSBEITRAG     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 KRTMONBEITRWKZ       PIC X(1).                                    
           10 KRTSELBSTBETEILIG    PIC S9(9)V9(2) USAGE COMP-3.                 
           10 KRTSELBSTBETWKZ      PIC X(1).                                    
           10 KRTTARIFNAMEN        PIC X(8).                                    
           10 KRTRISIKOZUSCHLAG    PIC X(10).                                   
           10 KRTVERSLEISTUNG      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 KRTVERSLSTWKZ        PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 KRTANWARTSCHAFT      PIC X(2).                                    
           10 KRTBE-REDUK-BTG      PIC S9(9)V9(2) USAGE COMP-3.                 
           10 KRT-ANZ-TAR-DUP      PIC S9(4) USAGE COMP.                        
           10 KRTTARIFART-KTY      PIC S9(4) USAGE COMP.                        
      ************************** ENDE-DCLGEN *************************          
