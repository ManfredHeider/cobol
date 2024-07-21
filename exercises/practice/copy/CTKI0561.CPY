      *****************************************************************         
CCC005*    EXEC SQL DECLARE G21V_VERTRAG TABLE                                  
CCC004*    ( G21VVVNR                       CHAR(15) NOT NULL,                  
CCC004*      G21VVMANDANT                   INTEGER NOT NULL,                   
CCC004*      G21VVVERARB_KZN                CHAR(1) NOT NULL,                   
CCC004*      G21VVVBNR                      INTEGER NOT NULL,                   
CCC004*      G21VVSPARTE                    CHAR(3) NOT NULL,                   
CCC004*      G21VVAENDKZN                   CHAR(1) NOT NULL,                   
CCC004*      G21VVVERTRZUSTAND              SMALLINT NOT NULL,                  
CCC004*      G21VVVERTRBEGINN               CHAR(10) NOT NULL,                  
CCC004*      G21VVVERTRABLAUF               CHAR(10) NOT NULL,                  
CCC004*      G21VVZAHLBEITRAG               DECIMAL(11, 2) NOT NULL,            
CCC004*      G21VVNJBEITRAG                 DECIMAL(11, 2) NOT NULL,            
CCC004*      G21VVZAHLWEISE                 SMALLINT NOT NULL,                  
CCC004*      G21VVPRODUKT                   CHAR(50) NOT NULL,                  
CCC004*      G21VVTARIF                     CHAR(50) NOT NULL,                  
CCC004*      G21VVRENTENBEZUG               CHAR(1) NOT NULL,                   
CCC004*      G21VVRENTEVORAUS               DECIMAL(7, 0) NOT NULL,             
CCC004*      G21VVRENTEGARANT               DECIMAL(7, 0) NOT NULL,             
CCC004*      G21VVRNTZAHLW                  SMALLINT NOT NULL,                  
CCC004*      G21VVLEISTUNG_KZN              CHAR(1) NOT NULL,                   
CCC004*      G21VVBERECH_DAT                CHAR(10) NOT NULL,                  
CCC004*      G21VVERLFALL_SUM               DECIMAL(11, 2) NOT NULL,            
CCC004*      G21VVTODFALL_SUM               DECIMAL(11, 2) NOT NULL,            
CCC004*      G21VVRNTFAKTOR                 DECIMAL(7, 2) NOT NULL,             
CCC004*      G21VVKAPABFIND                 DECIMAL(11, 2) NOT NULL,            
CCC004*      G21VVFONDSGUTH                 DECIMAL(11, 2) NOT NULL,            
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G21V_VERTRAG                *               
      ******************************************************************        
       01  G21V-VERTRAG-GESAMT.                                                 
        05 NI-G21V-VERTRAG-TAB.                                                 
           10 NI-G21V-VERTRAG-ELE PIC S9(04) COMP OCCURS 27 TIMES.              
        05 NI-G21V-VERTRAG REDEFINES NI-G21V-VERTRAG-TAB.                       
           10 NI-G21VVVNR           PIC S9(4) COMP.                             
           10 NI-G21VVMANDANT       PIC S9(4) COMP.                             
           10 NI-G21VVVERARB-KZN    PIC S9(4) COMP.                             
           10 NI-G21VVVBNR          PIC S9(4) COMP.                             
           10 NI-G21VVSPARTE        PIC S9(4) COMP.                             
           10 NI-G21VVAENDKZN       PIC S9(4) COMP.                             
           10 NI-G21VVVERTRZUSTAND  PIC S9(4) COMP.                             
           10 NI-G21VVVERTRBEGINN   PIC S9(4) COMP.                             
           10 NI-G21VVVERTRABLAUF   PIC S9(4) COMP.                             
           10 NI-G21VVZAHLBEITRAG   PIC S9(4) COMP.                             
           10 NI-G21VVNJBEITRAG     PIC S9(4) COMP.                             
           10 NI-G21VVZAHLWEISE     PIC S9(4) COMP.                             
           10 NI-G21VVPRODUKT       PIC S9(4) COMP.                             
           10 NI-G21VVTARIF         PIC S9(4) COMP.                             
           10 NI-G21VVRENTENBEZUG   PIC S9(4) COMP.                             
           10 NI-G21VVRENTEVORAUS   PIC S9(4) COMP.                             
           10 NI-G21VVRENTEGARANT   PIC S9(4) COMP.                             
           10 NI-G21VVRNTZAHLW      PIC S9(4) COMP.                             
           10 NI-G21VVLEISTUNG-KZN  PIC S9(4) COMP.                             
           10 NI-G21VVBERECH-DAT    PIC S9(4) COMP.                             
           10 NI-G21VVERLFALL-SUM   PIC S9(4) COMP.                             
           10 NI-G21VVTODFALL-SUM   PIC S9(4) COMP.                             
           10 NI-G21VVRNTFAKTOR     PIC S9(4) COMP.                             
           10 NI-G21VVKAPABFIND     PIC S9(4) COMP.                             
           10 NI-G21VVFONDSGUTH     PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-G21V-VERTRAG.                                                    
           10 G21VVVNR             PIC X(15).                                   
           10 G21VVMANDANT         PIC S9(9) USAGE COMP-5.                      
           10 G21VVVERARB-KZN      PIC X(1).                                    
           10 G21VVVBNR            PIC S9(9) USAGE COMP-5.                      
           10 G21VVSPARTE          PIC X(3).                                    
           10 G21VVAENDKZN         PIC X(1).                                    
           10 G21VVVERTRZUSTAND    PIC S9(4) USAGE COMP-5.                      
           10 G21VVVERTRBEGINN     PIC X(10).                                   
           10 G21VVVERTRABLAUF     PIC X(10).                                   
           10 G21VVZAHLBEITRAG     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G21VVNJBEITRAG       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G21VVZAHLWEISE       PIC S9(4) USAGE COMP-5.                      
           10 G21VVPRODUKT         PIC X(50).                                   
           10 G21VVTARIF           PIC X(50).                                   
           10 G21VVRENTENBEZUG     PIC X(1).                                    
           10 G21VVRENTEVORAUS     PIC S9(7)V USAGE COMP-3.                     
           10 G21VVRENTEGARANT     PIC S9(7)V USAGE COMP-3.                     
           10 G21VVRNTZAHLW        PIC S9(4) USAGE COMP-5.                      
           10 G21VVLEISTUNG-KZN    PIC X(1).                                    
           10 G21VVBERECH-DAT      PIC X(10).                                   
           10 G21VVERLFALL-SUM     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G21VVTODFALL-SUM     PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G21VVRNTFAKTOR       PIC S9(5)V9(2) USAGE COMP-3.                 
           10 G21VVKAPABFIND       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G21VVFONDSGUTH       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 LETZTERAENDERER      PIC X(10).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************          
