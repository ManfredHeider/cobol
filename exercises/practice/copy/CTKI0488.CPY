      *****************************************************************         
CCC005*    EXEC SQL DECLARE G79_KUNDEVERTRAG TABLE                              
CCC004*    ( G79VERTRAGSNR                  CHAR(15) NOT NULL,                  
CCC004*      G79MANDANT                     INTEGER NOT NULL,                   
CCC004*      G79ISIN                        CHAR(12) NOT NULL,                  
CCC004*      G79STATUS                      CHAR(1) NOT NULL,                   
CCC004*      G79VRTRGBEGINN                 CHAR(8) NOT NULL,                   
CCC004*      G79VRTRGENDE                   CHAR(8) NOT NULL,                   
CCC004*      G79ABSCHLVBNR                  INTEGER NOT NULL,                   
CCC004*      G79KUNDENDATEN                 VARCHAR(382) NOT NULL,              
CCC004*      G79SPARRATE                    DECIMAL(9, 2) NOT NULL,             
CCC004*      G79ZAHLUNGSART                 CHAR(1) NOT NULL,                   
CCC004*      G79ABBUCHTERMIN                CHAR(1) NOT NULL,                   
CCC004*      G79IBAN                        CHAR(34) NOT NULL,                  
CCC004*      G79BIC                         CHAR(11) NOT NULL,                  
CCC004*      G79ABWNAME                     CHAR(40) NOT NULL,                  
CCC004*      G79ENDEAUSDAT                  CHAR(8) NOT NULL,                   
CCC004*      G79RATESCHLDAT                 CHAR(8) NOT NULL,                   
CCC004*      G79BESTANDGRAMM                DECIMAL(11, 4) NOT NULL,            
CCC004*      G79BESTANDEURO                 DECIMAL(9, 2) NOT NULL,             
CCC004*      G79BUCHUNGSDAT                 CHAR(8) NOT NULL,                   
CCC004*      G79RUECKGRAMM                  DECIMAL(6, 2) NOT NULL,             
CCC004*      G79SUMMEEIN                    DECIMAL(9, 2) NOT NULL,             
CCC004*      G79SUMMEAUS                    DECIMAL(9, 2) NOT NULL,             
CCC004*      G79RABATT                      CHAR(1) NOT NULL,                   
CCC004*      G79KNDRABATT                   CHAR(1) NOT NULL,                   
CCC004*      G79KUNDENIDEXT                 INTEGER NOT NULL,                   
CCC004*      G79FLATRATE                    CHAR(1) NOT NULL,                   
CCC004*      G79RATEOEFFDAT                 CHAR(8) NOT NULL,                   
CCC004*      G79DYNAMIK                     DECIMAL(5, 2) NOT NULL,             
CCC004*      G79SPARRATEID                  INTEGER NOT NULL,                   
CCC004*      G79SPARRATELFDNR               SMALLINT NOT NULL,                  
CCC004*      G79KUMAUSLIEF1                 INTEGER NOT NULL,                   
CCC004*      G79KUMAUSLIEF2                 INTEGER NOT NULL,                   
CCC004*      G79KUMAUSLIEF3                 INTEGER NOT NULL                    
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G79_KUNDEVERTRAG            *               
      ******************************************************************        
       01  G79-KUNDEVERTRAG-GESAMT.                                             
        05 NI-G79-KUNDEVERTRAG-TAB.                                             
           10 NI-G79-KUNDEVERTRAG-ELE PIC S9(04) COMP OCCURS 33 TIMES.          
        05 NI-G79-KUNDEVERTRAG REDEFINES NI-G79-KUNDEVERTRAG-TAB.               
           10 NI-G79VERTRAGSNR      PIC S9(4) COMP.                             
           10 NI-G79MANDANT         PIC S9(4) COMP.                             
           10 NI-G79ISIN            PIC S9(4) COMP.                             
           10 NI-G79STATUS          PIC S9(4) COMP.                             
           10 NI-G79VRTRGBEGINN     PIC S9(4) COMP.                             
           10 NI-G79VRTRGENDE       PIC S9(4) COMP.                             
           10 NI-G79ABSCHLVBNR      PIC S9(4) COMP.                             
           10 NI-G79KUNDENDATEN     PIC S9(4) COMP.                             
           10 NI-G79SPARRATE        PIC S9(4) COMP.                             
           10 NI-G79ZAHLUNGSART     PIC S9(4) COMP.                             
           10 NI-G79ABBUCHTERMIN    PIC S9(4) COMP.                             
           10 NI-G79IBAN            PIC S9(4) COMP.                             
           10 NI-G79BIC             PIC S9(4) COMP.                             
           10 NI-G79ABWNAME         PIC S9(4) COMP.                             
           10 NI-G79ENDEAUSDAT      PIC S9(4) COMP.                             
           10 NI-G79RATESCHLDAT     PIC S9(4) COMP.                             
           10 NI-G79BESTANDGRAMM    PIC S9(4) COMP.                             
           10 NI-G79BESTANDEURO     PIC S9(4) COMP.                             
           10 NI-G79BUCHUNGSDAT     PIC S9(4) COMP.                             
           10 NI-G79RUECKGRAMM      PIC S9(4) COMP.                             
           10 NI-G79SUMMEEIN        PIC S9(4) COMP.                             
           10 NI-G79SUMMEAUS        PIC S9(4) COMP.                             
           10 NI-G79RABATT          PIC S9(4) COMP.                             
           10 NI-G79KNDRABATT       PIC S9(4) COMP.                             
           10 NI-G79KUNDENIDEXT     PIC S9(4) COMP.                             
           10 NI-G79FLATRATE        PIC S9(4) COMP.                             
           10 NI-G79RATEOEFFDAT     PIC S9(4) COMP.                             
           10 NI-G79DYNAMIK         PIC S9(4) COMP.                             
           10 NI-G79SPARRATEID      PIC S9(4) COMP.                             
           10 NI-G79SPARRATELFDNR   PIC S9(4) COMP.                             
           10 NI-G79KUMAUSLIEF1     PIC S9(4) COMP.                             
           10 NI-G79KUMAUSLIEF2     PIC S9(4) COMP.                             
           10 NI-G79KUMAUSLIEF3     PIC S9(4) COMP.                             
        05 DCL-G79-KUNDEVERTRAG.                                                
           10 G79VERTRAGSNR        PIC X(15).                                   
           10 G79MANDANT           PIC S9(9) USAGE COMP-5.                      
           10 G79ISIN              PIC X(12).                                   
           10 G79STATUS            PIC X(1).                                    
           10 G79VRTRGBEGINN       PIC X(8).                                    
           10 G79VRTRGENDE         PIC X(8).                                    
           10 G79ABSCHLVBNR        PIC S9(9) USAGE COMP-5.                      
           10 G79KUNDENDATEN.                                                   
              49 G79KUNDENDATEN-LEN                                             
                 PIC S9(4) USAGE COMP-5.                                        
              49 G79KUNDENDATEN-TEXT                                            
                 PIC X(382).                                                    
           10 G79SPARRATE          PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79ZAHLUNGSART       PIC X(1).                                    
           10 G79ABBUCHTERMIN      PIC X(1).                                    
           10 G79IBAN              PIC X(34).                                   
           10 G79BIC               PIC X(11).                                   
           10 G79ABWNAME           PIC X(40).                                   
           10 G79ENDEAUSDAT        PIC X(8).                                    
           10 G79RATESCHLDAT       PIC X(8).                                    
           10 G79BESTANDGRAMM      PIC S9(7)V9(4) USAGE COMP-3.                 
           10 G79BESTANDEURO       PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79BUCHUNGSDAT       PIC X(8).                                    
           10 G79RUECKGRAMM        PIC S9(4)V9(2) USAGE COMP-3.                 
           10 G79SUMMEEIN          PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79SUMMEAUS          PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G79RABATT            PIC X(1).                                    
           10 G79KNDRABATT         PIC X(1).                                    
           10 G79KUNDENIDEXT       PIC S9(9) USAGE COMP-5.                      
           10 G79FLATRATE          PIC X(1).                                    
           10 G79RATEOEFFDAT       PIC X(8).                                    
           10 G79DYNAMIK           PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G79SPARRATEID        PIC S9(9) USAGE COMP-5.                      
           10 G79SPARRATELFDNR     PIC S9(4) USAGE COMP-5.                      
           10 G79KUMAUSLIEF1       PIC S9(9) USAGE COMP-5.                      
           10 G79KUMAUSLIEF2       PIC S9(9) USAGE COMP-5.                      
           10 G79KUMAUSLIEF3       PIC S9(9) USAGE COMP-5.                      
      ************************** ENDE-DCLGEN *************************          
