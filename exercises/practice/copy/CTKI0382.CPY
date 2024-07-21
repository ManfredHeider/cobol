      *****************************************************************         
CCC005*    EXEC SQL DECLARE LBNPSVERTRAG TABLE                                  
CCC004*    ( LPVVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LPVPRODUKTID                   SMALLINT NOT NULL,                  
CCC004*      LPVSTVD0067_KTY                SMALLINT,                           
CCC004*      LPVSTVD0067_KNR                DECIMAL(15, 0),                     
CCC004*      LPVPRODKLAS_KTY                SMALLINT NOT NULL,                  
CCC004*      LPVPRODKLAS_KNR                DECIMAL(15, 0) NOT NULL,            
CCC004*      LPVGSMTTAR_BEZ                 CHAR(25) NOT NULL,                  
CCC004*      LPVWERBEBEZ_KTY                SMALLINT NOT NULL,                  
CCC004*      LPVWERBEBEZ_KNR                DECIMAL(15, 0) NOT NULL,            
CCC004*      LPVRNTBEZUG_KZN                CHAR(1),                            
CCC004*      LPVBAV_KZN                     CHAR(1) NOT NULL,                   
CCC004*      LPVVZUST_D_KTY                 SMALLINT NOT NULL,                  
CCC004*      LPVVZUST_D_KNR                 SMALLINT NOT NULL,                  
CCC004*      LPVVZUST_D_DAT                 DATE                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE LBNPSVERTRAG                *               
      ******************************************************************        
       01  LBNPSVERTRAG-GESAMT.                                                 
        05 NI-LBNPSVERTRAG-TAB.                                                 
           10 NI-LBNPSVERTRAG-ELE PIC S9(04) COMP OCCURS 15 TIMES.              
        05 NI-LBNPSVERTRAG REDEFINES NI-LBNPSVERTRAG-TAB.                       
           10 NI-LPVVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LPVPRODUKTID       PIC S9(4) COMP.                             
           10 NI-LPVSTVD0067-KTY    PIC S9(4) COMP.                             
           10 NI-LPVSTVD0067-KNR    PIC S9(4) COMP.                             
           10 NI-LPVPRODKLAS-KTY    PIC S9(4) COMP.                             
           10 NI-LPVPRODKLAS-KNR    PIC S9(4) COMP.                             
           10 NI-LPVGSMTTAR-BEZ     PIC S9(4) COMP.                             
           10 NI-LPVWERBEBEZ-KTY    PIC S9(4) COMP.                             
           10 NI-LPVWERBEBEZ-KNR    PIC S9(4) COMP.                             
           10 NI-LPVRNTBEZUG-KZN    PIC S9(4) COMP.                             
           10 NI-LPVBAV-KZN         PIC S9(4) COMP.                             
           10 NI-LPVVZUST-D-KTY     PIC S9(4) COMP.                             
           10 NI-LPVVZUST-D-KNR     PIC S9(4) COMP.                             
           10 NI-LPVVZUST-D-DAT     PIC S9(4) COMP.                             
        05 DCL-LBNPSVERTRAG.                                                    
           10 LPVVERTRAGSID        PIC X(13).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LPVPRODUKTID         PIC S9(4) USAGE COMP-5.                      
           10 LPVSTVD0067-KTY      PIC S9(4) USAGE COMP-5.                      
           10 LPVSTVD0067-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 LPVPRODKLAS-KTY      PIC S9(4) USAGE COMP-5.                      
           10 LPVPRODKLAS-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 LPVGSMTTAR-BEZ       PIC X(25).                                   
           10 LPVWERBEBEZ-KTY      PIC S9(4) USAGE COMP-5.                      
           10 LPVWERBEBEZ-KNR      PIC S9(15)V USAGE COMP-3.                    
           10 LPVRNTBEZUG-KZN      PIC X(1).                                    
           10 LPVBAV-KZN           PIC X(1).                                    
           10 LPVVZUST-D-KTY       PIC S9(4) USAGE COMP-5.                      
           10 LPVVZUST-D-KNR       PIC S9(4) USAGE COMP-5.                      
           10 LPVVZUST-D-DAT       PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
