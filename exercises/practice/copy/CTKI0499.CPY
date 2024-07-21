      *****************************************************************         
CCC005*    EXEC SQL DECLARE G79_DIREKTKAUF TABLE                                
CCC004*    ( G79DVERTRAGSNR                 CHAR(15) NOT NULL,                  
CCC004*      G79DMANDANT                    INTEGER NOT NULL,                   
CCC004*      G79DABSCHLVBNR                 INTEGER NOT NULL,                   
CCC004*      G79DNACHNAME                   CHAR(30) NOT NULL,                  
CCC004*      G79DVORNAME                    CHAR(30) NOT NULL,                  
CCC004*      G79DFIRMENNAME                 CHAR(70) NOT NULL,                  
CCC004*      G79DGEBDAT                     CHAR(8) NOT NULL,                   
CCC004*      G79DANREDE                     CHAR(1) NOT NULL,                   
CCC004*      G79DSTRHNR                     CHAR(40) NOT NULL,                  
CCC004*      G79DPLZ                        CHAR(9) NOT NULL,                   
CCC004*      G79DORT                        CHAR(30) NOT NULL,                  
CCC004*      G79DLAND                       CHAR(3) NOT NULL,                   
CCC004*      G79DEMAIL                      CHAR(40) NOT NULL,                  
CCC004*      G79DKUNDENID                   INTEGER NOT NULL,                   
CCC004*      G79DGESID                      SMALLINT NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G79_DIREKTKAUF              *               
      ******************************************************************        
       01  G79-DIREKTKAUF-GESAMT.                                               
        05 NI-G79-DIREKTKAUF-TAB.                                               
           10 NI-G79-DIREKTKAUF-ELE PIC S9(04) COMP OCCURS 15 TIMES.            
        05 NI-G79-DIREKTKAUF REDEFINES NI-G79-DIREKTKAUF-TAB.                   
           10 NI-G79DVERTRAGSNR     PIC S9(4) COMP.                             
           10 NI-G79DMANDANT        PIC S9(4) COMP.                             
           10 NI-G79DABSCHLVBNR     PIC S9(4) COMP.                             
           10 NI-G79DNACHNAME       PIC S9(4) COMP.                             
           10 NI-G79DVORNAME        PIC S9(4) COMP.                             
           10 NI-G79DFIRMENNAME     PIC S9(4) COMP.                             
           10 NI-G79DGEBDAT         PIC S9(4) COMP.                             
           10 NI-G79DANREDE         PIC S9(4) COMP.                             
           10 NI-G79DSTRHNR         PIC S9(4) COMP.                             
           10 NI-G79DPLZ            PIC S9(4) COMP.                             
           10 NI-G79DORT            PIC S9(4) COMP.                             
           10 NI-G79DLAND           PIC S9(4) COMP.                             
           10 NI-G79DEMAIL          PIC S9(4) COMP.                             
           10 NI-G79DKUNDENID       PIC S9(4) COMP.                             
           10 NI-G79DGESID          PIC S9(4) COMP.                             
        05 DCL-G79-DIREKTKAUF.                                                  
           10 G79DVERTRAGSNR       PIC X(15).                                   
           10 G79DMANDANT          PIC S9(9) USAGE COMP-5.                      
           10 G79DABSCHLVBNR       PIC S9(9) USAGE COMP-5.                      
           10 G79DNACHNAME         PIC X(30).                                   
           10 G79DVORNAME          PIC X(30).                                   
           10 G79DFIRMENNAME       PIC X(70).                                   
           10 G79DGEBDAT           PIC X(8).                                    
           10 G79DANREDE           PIC X(1).                                    
           10 G79DSTRHNR           PIC X(40).                                   
           10 G79DPLZ              PIC X(9).                                    
           10 G79DORT              PIC X(30).                                   
           10 G79DLAND             PIC X(3).                                    
           10 G79DEMAIL            PIC X(40).                                   
           10 G79DKUNDENID         PIC S9(9) USAGE COMP-5.                      
           10 G79DGESID            PIC S9(4) USAGE COMP-5.                      
      ************************** ENDE-DCLGEN *************************          
