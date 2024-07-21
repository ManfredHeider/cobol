      *****************************************************************         
CCC005*    EXEC SQL DECLARE LEBENFOND TABLE                                     
CCC004*    ( LFOVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LFOVERSTARLFDNR                SMALLINT NOT NULL,                  
CCC004*      LFOLFDNR                       SMALLINT NOT NULL,                  
CCC004*      LFOANZAHLANTEILE               DECIMAL(14, 6),                     
CCC004*      LFORUECKNAHMEBETR              DECIMAL(13, 2),                     
CCC004*      LFOPROZSPARBETR                DECIMAL(5, 2),                      
CCC004*      LFODECKUNGSKAPITAL             DECIMAL(13, 2),                     
CCC004*      LFODECKKAPITWKZ                CHAR(1) NOT NULL,                   
CCC004*      LETZTEAENDERUNG                TIMESTAMP,                          
CCC004*      LFOFONDSKUERZEL                CHAR(2) NOT NULL,                   
CCC004*      LFOANZAHLANTBKZ                CHAR(1),                            
CCC004*      LFORUECKNAHMEBKZ               CHAR(1),                            
CCC004*      LFOPROZSPARBKZ                 CHAR(1),                            
CCC004*      LFODECKKAPITBKZ                CHAR(1),                            
CCC004*      LFOFONDSKUEBKZ                 CHAR(1)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE LEBENFOND                   *               
      ******************************************************************        
       01  LEBENFOND-GESAMT.                                                    
        05 NI-LEBENFOND-TAB.                                                    
           10 NI-LEBENFOND-ELE PIC S9(04) COMP OCCURS 15 TIMES.                 
        05 NI-LEBENFOND REDEFINES NI-LEBENFOND-TAB.                             
           10 NI-LFOVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LFOVERSTARLFDNR    PIC S9(4) COMP.                             
           10 NI-LFOLFDNR           PIC S9(4) COMP.                             
           10 NI-LFOANZAHLANTEILE   PIC S9(4) COMP.                             
           10 NI-LFORUECKNAHMEBETR  PIC S9(4) COMP.                             
           10 NI-LFOPROZSPARBETR    PIC S9(4) COMP.                             
           10 NI-LFODECKUNGSKAPITAL PIC S9(4) COMP.                             
           10 NI-LFODECKKAPITWKZ    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LFOFONDSKUERZEL    PIC S9(4) COMP.                             
           10 NI-LFOANZAHLANTBKZ    PIC S9(4) COMP.                             
           10 NI-LFORUECKNAHMEBKZ   PIC S9(4) COMP.                             
           10 NI-LFOPROZSPARBKZ     PIC S9(4) COMP.                             
           10 NI-LFODECKKAPITBKZ    PIC S9(4) COMP.                             
           10 NI-LFOFONDSKUEBKZ     PIC S9(4) COMP.                             
        05 DCL-LEBENFOND.                                                       
           10 LFOVERTRAGSID        PIC X(13).                                   
           10 LFOVERSTARLFDNR      PIC S9(4) USAGE COMP-5.                      
           10 LFOLFDNR             PIC S9(4) USAGE COMP-5.                      
           10 LFOANZAHLANTEILE     PIC S9(8)V9(6) USAGE COMP-3.                 
           10 LFORUECKNAHMEBETR    PIC S9(11)V9(2) USAGE COMP-3.                
           10 LFOPROZSPARBETR      PIC S9(3)V9(2) USAGE COMP-3.                 
           10 LFODECKUNGSKAPITAL   PIC S9(11)V9(2) USAGE COMP-3.                
           10 LFODECKKAPITWKZ      PIC X(1).                                    
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LFOFONDSKUERZEL      PIC X(2).                                    
           10 LFOANZAHLANTBKZ      PIC X(1).                                    
           10 LFORUECKNAHMEBKZ     PIC X(1).                                    
           10 LFOPROZSPARBKZ       PIC X(1).                                    
           10 LFODECKKAPITBKZ      PIC X(1).                                    
           10 LFOFONDSKUEBKZ       PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
