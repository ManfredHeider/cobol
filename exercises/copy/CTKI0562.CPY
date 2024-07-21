      *****************************************************************         
CCC005*    EXEC SQL DECLARE G21V_KUNDE TABLE                                    
CCC004*    ( G21VKVNR                       CHAR(15) NOT NULL,                  
CCC004*      G21VKVERARB_KZN                CHAR(1) NOT NULL,                   
CCC004*      G21VKNACHNAME                  CHAR(80) NOT NULL,                  
CCC004*      G21VKVORNAME                   CHAR(30) NOT NULL,                  
CCC004*      G21VKGEBDAT                    CHAR(10) NOT NULL,                  
CCC004*      G21VKANREDE                    SMALLINT NOT NULL,                  
CCC004*      G21VKGESCHLECHT                CHAR(1) NOT NULL,                   
CCC004*      G21VKSTR                       CHAR(45) NOT NULL,                  
CCC004*      G21VKPLZ                       CHAR(15) NOT NULL,                  
CCC004*      G21VKORT                       CHAR(30) NOT NULL,                  
CCC004*      G21VKLKZN                      CHAR(3) NOT NULL,                   
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G21V_KUNDE                  *               
      ******************************************************************        
       01  G21V-KUNDE-GESAMT.                                                   
        05 NI-G21V-KUNDE-TAB.                                                   
           10 NI-G21V-KUNDE-ELE PIC S9(04) COMP OCCURS 13 TIMES.                
        05 NI-G21V-KUNDE REDEFINES NI-G21V-KUNDE-TAB.                           
           10 NI-G21VKVNR           PIC S9(4) COMP.                             
           10 NI-G21VKVERARB-KZN    PIC S9(4) COMP.                             
           10 NI-G21VKNACHNAME      PIC S9(4) COMP.                             
           10 NI-G21VKVORNAME       PIC S9(4) COMP.                             
           10 NI-G21VKGEBDAT        PIC S9(4) COMP.                             
           10 NI-G21VKANREDE        PIC S9(4) COMP.                             
           10 NI-G21VKGESCHLECHT    PIC S9(4) COMP.                             
           10 NI-G21VKSTR           PIC S9(4) COMP.                             
           10 NI-G21VKPLZ           PIC S9(4) COMP.                             
           10 NI-G21VKORT           PIC S9(4) COMP.                             
           10 NI-G21VKLKZN          PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-G21V-KUNDE.                                                      
           10 G21VKVNR             PIC X(15).                                   
           10 G21VKVERARB-KZN      PIC X(1).                                    
           10 G21VKNACHNAME        PIC X(80).                                   
           10 G21VKVORNAME         PIC X(30).                                   
           10 G21VKGEBDAT          PIC X(10).                                   
           10 G21VKANREDE          PIC S9(4) USAGE COMP-5.                      
           10 G21VKGESCHLECHT      PIC X(1).                                    
           10 G21VKSTR             PIC X(45).                                   
           10 G21VKPLZ             PIC X(15).                                   
           10 G21VKORT             PIC X(30).                                   
           10 G21VKLKZN            PIC X(3).                                    
           10 LETZTERAENDERER      PIC X(10).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************          
