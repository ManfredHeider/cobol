      *****************************************************************         
CCC005*    EXEC SQL DECLARE G79_GOLDRENTE TABLE                                 
CCC004*    ( G79GRVNR                       CHAR(15) NOT NULL,                  
CCC004*      G79GRMANDANT                   INTEGER NOT NULL,                   
CCC004*      G79GRISIN                      CHAR(12) NOT NULL,                  
CCC004*      G79GRSTATUS                    CHAR(1) NOT NULL,                   
CCC004*      G79GRBEGINNDAT                 CHAR(8) NOT NULL,                   
CCC004*      G79GRENDEDAT                   CHAR(8) NOT NULL,                   
CCC004*      G79GRABSCHLVBNR                INTEGER NOT NULL,                   
CCC004*      G79GRRABATTKZN                 CHAR(1) NOT NULL,                   
CCC004*      G79GRKNDRABATT                 CHAR(1) NOT NULL,                   
CCC004*      G79GRKUNDENIDEXT               INTEGER NOT NULL,                   
CCC004*      G79GRFLATRATE                  CHAR(1) NOT NULL,                   
CCC004*      G79GRKUNDENDATEN               VARCHAR(382) NOT NULL,              
CCC004*      G79GRSPARRATE                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G79GRZAHLUNGSART               CHAR(1) NOT NULL,                   
CCC004*      G79GRABBUCHTAG                 CHAR(1) NOT NULL,                   
CCC004*      G79GRIBAN                      CHAR(34) NOT NULL,                  
CCC004*      G79GRBIC                       CHAR(11) NOT NULL,                  
CCC004*      G79GRABWNAME                   CHAR(40) NOT NULL,                  
CCC004*      G79GRRATEOEFFDAT               CHAR(8) NOT NULL,                   
CCC004*      G79GRENDEAUSDAT                CHAR(8) NOT NULL,                   
CCC004*      G79GRRATESCHLDAT               CHAR(8) NOT NULL,                   
CCC004*      G79GRDYNAMIK                   DECIMAL(5, 2) NOT NULL,             
CCC004*      G79GRSPARRATEID                INTEGER NOT NULL,                   
CCC004*      G79GRSPARRATELFDNR             SMALLINT NOT NULL,                  
CCC004*      G79GRBESTGRAMM                 DECIMAL(11, 4) NOT NULL,            
CCC004*      G79GRBESTEURO                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G79GRBUCHUNGSDAT               CHAR(8) NOT NULL,                   
CCC004*      G79GRRUECKGRAMM                DECIMAL(6, 2) NOT NULL,             
CCC004*      G79GRSUMMEEIN                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G79GRSUMMEAUS                  DECIMAL(9, 2) NOT NULL,             
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL                  
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G79_GOLDRENTE               *               
      ******************************************************************        
       01  G79-GOLDRENTE-GESAMT.                                                
        05 NI-G79-GOLDRENTE-TAB.                                                
           10 NI-G79-GOLDRENTE-ELE PIC S9(04) COMP OCCURS 32 TIMES.             
        05 NI-G79-GOLDRENTE REDEFINES NI-G79-GOLDRENTE-TAB.                     
           10 NI-G79GRVNR           PIC S9(4) COMP.                             
           10 NI-G79GRMANDANT       PIC S9(4) COMP.                             
           10 NI-G79GRISIN          PIC S9(4) COMP.                             
           10 NI-G79GRSTATUS        PIC S9(4) COMP.                             
           10 NI-G79GRBEGINNDAT     PIC S9(4) COMP.                             
           10 NI-G79GRENDEDAT       PIC S9(4) COMP.                             
           10 NI-G79GRABSCHLVBNR    PIC S9(4) COMP.                             
           10 NI-G79GRRABATTKZN     PIC S9(4) COMP.                             
           10 NI-G79GRKNDRABATT     PIC S9(4) COMP.                             
           10 NI-G79GRKUNDENIDEXT   PIC S9(4) COMP.                             
           10 NI-G79GRFLATRATE      PIC S9(4) COMP.                             
           10 NI-G79GRKUNDENDATEN   PIC S9(4) COMP.                             
           10 NI-G79GRSPARRATE      PIC S9(4) COMP.                             
           10 NI-G79GRZAHLUNGSART   PIC S9(4) COMP.                             
           10 NI-G79GRABBUCHTAG     PIC S9(4) COMP.                             
           10 NI-G79GRIBAN          PIC S9(4) COMP.                             
           10 NI-G79GRBIC           PIC S9(4) COMP.                             
           10 NI-G79GRABWNAME       PIC S9(4) COMP.                             
           10 NI-G79GRRATEOEFFDAT   PIC S9(4) COMP.                             
           10 NI-G79GRENDEAUSDAT    PIC S9(4) COMP.                             
           10 NI-G79GRRATESCHLDAT   PIC S9(4) COMP.                             
           10 NI-G79GRDYNAMIK       PIC S9(4) COMP.                             
           10 NI-G79GRSPARRATEID    PIC S9(4) COMP.                             
           10 NI-G79GRSPARRATELFDNR PIC S9(4) COMP.                             
           10 NI-G79GRBESTGRAMM     PIC S9(4) COMP.                             
           10 NI-G79GRBESTEURO      PIC S9(4) COMP.                             
           10 NI-G79GRBUCHUNGSDAT   PIC S9(4) COMP.                             
           10 NI-G79GRRUECKGRAMM    PIC S9(4) COMP.                             
           10 NI-G79GRSUMMEEIN      PIC S9(4) COMP.                             
           10 NI-G79GRSUMMEAUS      PIC S9(4) COMP.                             
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
        05 DCL-G79-GOLDRENTE.                                                   
           10 G79GRVNR             PIC X(15).                                   
           10 G79GRMANDANT         PIC S9(9) USAGE COMP-5.                      
           10 G79GRISIN            PIC X(12).                                   
           10 G79GRSTATUS          PIC X(1).                                    
           10 G79GRBEGINNDAT       PIC X(8).                                    
           10 G79GRENDEDAT         PIC X(8).                                    
           10 G79GRABSCHLVBNR      PIC S9(9) USAGE COMP-5.                      
           10 G79GRRABATTKZN       PIC X(1).                                    
           10 G79GRKNDRABATT       PIC X(1).                                    
           10 G79GRKUNDENIDEXT     PIC S9(9) USAGE COMP-5.                      
           10 G79GRFLATRATE        PIC X(1).                                    
           10 G79GRKUNDENDATEN.                                                 
              49 G79GRKUNDENDATEN-LEN                                           
                 PIC S9(4) USAGE COMP-5.                                        
              49 G79GRKUNDENDATEN-TEXT                                          
                 PIC X(382).                                                    
           10 G79GRSPARRATE        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79GRZAHLUNGSART     PIC X(1).                                    
           10 G79GRABBUCHTAG       PIC X(1).                                    
           10 G79GRIBAN            PIC X(34).                                   
           10 G79GRBIC             PIC X(11).                                   
           10 G79GRABWNAME         PIC X(40).                                   
           10 G79GRRATEOEFFDAT     PIC X(8).                                    
           10 G79GRENDEAUSDAT      PIC X(8).                                    
           10 G79GRRATESCHLDAT     PIC X(8).                                    
           10 G79GRDYNAMIK         PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G79GRSPARRATEID      PIC S9(9) USAGE COMP-5.                      
           10 G79GRSPARRATELFDNR   PIC S9(4) USAGE COMP-5.                      
           10 G79GRBESTGRAMM       PIC S9(7)V9(4) USAGE COMP-3.                 
           10 G79GRBESTEURO        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79GRBUCHUNGSDAT     PIC X(8).                                    
           10 G79GRRUECKGRAMM      PIC S9(4)V9(2) USAGE COMP-3.                 
           10 G79GRSUMMEEIN        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79GRSUMMEAUS        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 LETZTERAENDERER      PIC X(10).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
      ************************** ENDE-DCLGEN *************************