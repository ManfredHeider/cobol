      *****************************************************************         
CCC005*    EXEC SQL DECLARE ASCLUB_KLB_FONDS TABLE                              
CCC004*    ( AKFFONDSID                     SMALLINT NOT NULL,                  
CCC004*      AKFGESID                       SMALLINT NOT NULL,                  
CCC004*      AKFWKN                         CHAR(6) NOT NULL,                   
CCC004*      AKFISIN                        CHAR(20) NOT NULL,                  
CCC004*      AKFVLKZN                       CHAR(1) NOT NULL,                   
CCC004*      AKFBEZEICHNUNG                 CHAR(40) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE ASCLUB_KLB_FONDS            *               
      ******************************************************************        
       01  ASCLUB-KLB-FONDS-GESAMT.                                             
        05 NI-ASCLUB-KLB-FONDS-TAB.                                             
           10 NI-ASCLUB-KLB-FONDS-ELE PIC S9(04) COMP OCCURS 7 TIMES.           
        05 NI-ASCLUB-KLB-FONDS REDEFINES NI-ASCLUB-KLB-FONDS-TAB.               
           10 NI-AKFFONDSID         PIC S9(4) COMP.                             
           10 NI-AKFGESID           PIC S9(4) COMP.                             
           10 NI-AKFWKN             PIC S9(4) COMP.                             
           10 NI-AKFISIN            PIC S9(4) COMP.                             
           10 NI-AKFVLKZN           PIC S9(4) COMP.                             
           10 NI-AKFBEZEICHNUNG     PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-ASCLUB-KLB-FONDS.                                                
           10 AKFFONDSID           PIC S9(4) USAGE COMP.                        
           10 AKFGESID             PIC S9(4) USAGE COMP.                        
           10 AKFWKN               PIC X(6).                                    
           10 AKFISIN              PIC X(20).                                   
           10 AKFVLKZN             PIC X(1).                                    
           10 AKFBEZEICHNUNG       PIC X(40).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************          
