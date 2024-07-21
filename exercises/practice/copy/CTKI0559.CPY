      *****************************************************************         
CCC005*    EXEC SQL DECLARE GEIGERVERTRZUSATZ TABLE                             
CCC004*    ( GVZVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      GVZKUNDENIDEXT                 INTEGER,                            
CCC004*      GVZFLATRATE                    CHAR(1),                            
CCC004*      GVZKNDRABATT                   DECIMAL(5, 2),                      
CCC004*      GVZKUMAUSLIEFGB                INTEGER,                            
CCC004*      GVZKUMAUSLIEFGM                INTEGER,                            
CCC004*      GVZKUMAUSLIEFSBB               INTEGER,                            
CCC004*      GVZKUMAUSLIEFSBZ               INTEGER,                            
CCC004*      GVZKUMAUSLIEFSM                INTEGER,                            
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE GEIGERVERTRZUSATZ           *               
      ******************************************************************        
       01  GEIGERVERTRZUSATZ-GESAMT.                                            
        05 NI-GEIGERVERTRZUSATZ-TAB.                                            
           10 NI-GEIGERVERTRZUSATZ-ELE PIC S9(04) COMP OCCURS 10 TIMES.         
        05 NI-GEIGERVERTRZUSATZ REDEFINES NI-GEIGERVERTRZUSATZ-TAB.             
           10 NI-GVZVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-GVZKUNDENIDEXT     PIC S9(4) COMP.                             
           10 NI-GVZFLATRATE        PIC S9(4) COMP.                             
           10 NI-GVZKNDRABATT       PIC S9(4) COMP.                             
           10 NI-GVZKUMAUSLIEFGB    PIC S9(4) COMP.                             
           10 NI-GVZKUMAUSLIEFGM    PIC S9(4) COMP.                             
           10 NI-GVZKUMAUSLIEFSBB   PIC S9(4) COMP.                             
           10 NI-GVZKUMAUSLIEFSBZ   PIC S9(4) COMP.                             
           10 NI-GVZKUMAUSLIEFSM    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-GEIGERVERTRZUSATZ.                                               
           10 GVZVERTRAGSID        PIC X(13).                                   
           10 GVZKUNDENIDEXT       PIC S9(9) USAGE COMP-5.                      
           10 GVZFLATRATE          PIC X(1).                                    
           10 GVZKNDRABATT         PIC S9(3)V9(2) USAGE COMP-3.                 
           10 GVZKUMAUSLIEFGB      PIC S9(9) USAGE COMP-5.                      
           10 GVZKUMAUSLIEFGM      PIC S9(9) USAGE COMP-5.                      
           10 GVZKUMAUSLIEFSBB     PIC S9(9) USAGE COMP-5.                      
           10 GVZKUMAUSLIEFSBZ     PIC S9(9) USAGE COMP-5.                      
           10 GVZKUMAUSLIEFSM      PIC S9(9) USAGE COMP-5.                      
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************          
