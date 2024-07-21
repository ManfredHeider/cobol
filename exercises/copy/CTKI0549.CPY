      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  09/02/21                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CTKI0549.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/CTKI0549.CPv  $                                    
      *                                                                         
      *   Rev 2.1   Feb 28 2021 10:44:42   t13ex39                              
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT                           
      *                                                                         
      *   Rev 2.0   Feb 9 2021 06:30:37   T13EX39                               
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
CCC005*    EXEC SQL DECLARE G48_SPARPLAN TABLE                                  
CCC004*    ( G48SPVNR                       CHAR(15) NOT NULL,                  
CCC004*      G48SPPLANNR                    CHAR(2) NOT NULL,                   
CCC004*      G48SPWAEHRUNGSKZ               CHAR(3) NOT NULL,                   
CCC004*      G48SPDEPOTUNTERNR              CHAR(2) NOT NULL,                   
CCC004*      G48SPOFFNUNGSDATUM             CHAR(8) NOT NULL,                   
CCC004*      G48SPSCHLIESSDATUM             CHAR(8) NOT NULL,                   
CCC004*      G48SPABWEICHKTOINH             CHAR(27) NOT NULL,                  
CCC004*      G48SPWKN                       CHAR(6) NOT NULL,                   
CCC004*      G48SPISIN                      CHAR(12) NOT NULL,                  
CCC004*      G48SPSPARRATE                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G48SPPROZENTDYN                DECIMAL(4, 2) NOT NULL,             
CCC004*      G48SPZAHLWEISE                 CHAR(1) NOT NULL,                   
CCC004*      G48SPTAGEINZAHLUNG             SMALLINT NOT NULL,                  
CCC004*      G48SPSPARPLENDEVL              CHAR(8) NOT NULL,                   
CCC004*      G48SPVERTRAGSTYP               CHAR(2) NOT NULL,                   
CCC004*      G48SPBEGRATENAUS               CHAR(8) NOT NULL,                   
CCC004*      G48SPENDERATENAUS              CHAR(8) NOT NULL,                   
CCC004*      G48SPDELKZ                     CHAR(1) NOT NULL,                   
CCC004*      G48SPKOSTENPROZENT             DECIMAL(3, 2) NOT NULL,             
CCC004*      G48SPURGESAMTEURO              DECIMAL(8, 2) NOT NULL,             
CCC004*      G48SPURSPARRATE                DECIMAL(6, 2) NOT NULL,             
CCC004*      G48SPAUSGFOND                  DECIMAL(4, 2) NOT NULL,             
CCC004*      G48SPAUSGAKTFOND               DECIMAL(4, 2) NOT NULL,             
CCC004*      G48SPVERTRAGLFZ                SMALLINT NOT NULL,                  
CCC004*      G48SPBEGINNDYN                 CHAR(8) NOT NULL,                   
CCC004*      G48SPZLASTRUECK                SMALLINT NOT NULL,                  
CCC004*      G48SPKUENDMERKMAL              CHAR(1) NOT NULL,                   
CCC004*      G48SPDATVERTKUEND              CHAR(8) NOT NULL,                   
CCC004*      G48SPBONIFIKATION              DECIMAL(7, 4) NOT NULL,             
CCC004*      G48SPURSPRVERTNR               CHAR(12) NOT NULL,                  
CCC004*      G48SPPIN                       DECIMAL(9, 0) NOT NULL,             
CCC004*      G48SPIBAN                      CHAR(34) NOT NULL,                  
CCC004*      G48SPBIC                       CHAR(11) NOT NULL,                  
CCC004*      G48SPMANDAT                    CHAR(35) NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G48_SPARPLAN                *               
      ******************************************************************        
       01  G48-SPARPLAN-GESAMT.                                                 
        05 NI-G48-SPARPLAN-TAB.                                                 
           10 NI-G48-SPARPLAN-ELE PIC S9(04) COMP OCCURS 34 TIMES.              
        05 NI-G48-SPARPLAN REDEFINES NI-G48-SPARPLAN-TAB.                       
           10 NI-G48SPVNR           PIC S9(4) COMP.                             
           10 NI-G48SPPLANNR        PIC S9(4) COMP.                             
           10 NI-G48SPWAEHRUNGSKZ   PIC S9(4) COMP.                             
           10 NI-G48SPDEPOTUNTERNR  PIC S9(4) COMP.                             
           10 NI-G48SPOFFNUNGSDATUM PIC S9(4) COMP.                             
           10 NI-G48SPSCHLIESSDATUM PIC S9(4) COMP.                             
           10 NI-G48SPABWEICHKTOINH PIC S9(4) COMP.                             
           10 NI-G48SPWKN           PIC S9(4) COMP.                             
           10 NI-G48SPISIN          PIC S9(4) COMP.                             
           10 NI-G48SPSPARRATE      PIC S9(4) COMP.                             
           10 NI-G48SPPROZENTDYN    PIC S9(4) COMP.                             
           10 NI-G48SPZAHLWEISE     PIC S9(4) COMP.                             
           10 NI-G48SPTAGEINZAHLUNG PIC S9(4) COMP.                             
           10 NI-G48SPSPARPLENDEVL  PIC S9(4) COMP.                             
           10 NI-G48SPVERTRAGSTYP   PIC S9(4) COMP.                             
           10 NI-G48SPBEGRATENAUS   PIC S9(4) COMP.                             
           10 NI-G48SPENDERATENAUS  PIC S9(4) COMP.                             
           10 NI-G48SPDELKZ         PIC S9(4) COMP.                             
           10 NI-G48SPKOSTENPROZENT PIC S9(4) COMP.                             
           10 NI-G48SPURGESAMTEURO  PIC S9(4) COMP.                             
           10 NI-G48SPURSPARRATE    PIC S9(4) COMP.                             
           10 NI-G48SPAUSGFOND      PIC S9(4) COMP.                             
           10 NI-G48SPAUSGAKTFOND   PIC S9(4) COMP.                             
           10 NI-G48SPVERTRAGLFZ    PIC S9(4) COMP.                             
           10 NI-G48SPBEGINNDYN     PIC S9(4) COMP.                             
           10 NI-G48SPZLASTRUECK    PIC S9(4) COMP.                             
           10 NI-G48SPKUENDMERKMAL  PIC S9(4) COMP.                             
           10 NI-G48SPDATVERTKUEND  PIC S9(4) COMP.                             
           10 NI-G48SPBONIFIKATION  PIC S9(4) COMP.                             
           10 NI-G48SPURSPRVERTNR   PIC S9(4) COMP.                             
           10 NI-G48SPPIN           PIC S9(4) COMP.                             
           10 NI-G48SPIBAN          PIC S9(4) COMP.                             
           10 NI-G48SPBIC           PIC S9(4) COMP.                             
           10 NI-G48SPMANDAT        PIC S9(4) COMP.                             
        05 DCL-G48-SPARPLAN.                                                    
           10 G48SPVNR             PIC X(15).                                   
           10 G48SPPLANNR          PIC X(2).                                    
           10 G48SPWAEHRUNGSKZ     PIC X(3).                                    
           10 G48SPDEPOTUNTERNR    PIC X(2).                                    
           10 G48SPOFFNUNGSDATUM   PIC X(8).                                    
           10 G48SPSCHLIESSDATUM   PIC X(8).                                    
           10 G48SPABWEICHKTOINH   PIC X(27).                                   
           10 G48SPWKN             PIC X(6).                                    
           10 G48SPISIN            PIC X(12).                                   
           10 G48SPSPARRATE        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G48SPPROZENTDYN      PIC S9(2)V9(2) USAGE COMP-3.                 
           10 G48SPZAHLWEISE       PIC X(1).                                    
           10 G48SPTAGEINZAHLUNG   PIC S9(4) USAGE COMP.                        
           10 G48SPSPARPLENDEVL    PIC X(8).                                    
           10 G48SPVERTRAGSTYP     PIC X(2).                                    
           10 G48SPBEGRATENAUS     PIC X(8).                                    
           10 G48SPENDERATENAUS    PIC X(8).                                    
           10 G48SPDELKZ           PIC X(1).                                    
           10 G48SPKOSTENPROZENT   PIC S9(1)V9(2) USAGE COMP-3.                 
           10 G48SPURGESAMTEURO    PIC S9(6)V9(2) USAGE COMP-3.                 
           10 G48SPURSPARRATE      PIC S9(4)V9(2) USAGE COMP-3.                 
           10 G48SPAUSGFOND        PIC S9(2)V9(2) USAGE COMP-3.                 
           10 G48SPAUSGAKTFOND     PIC S9(2)V9(2) USAGE COMP-3.                 
           10 G48SPVERTRAGLFZ      PIC S9(4) USAGE COMP.                        
           10 G48SPBEGINNDYN       PIC X(8).                                    
           10 G48SPZLASTRUECK      PIC S9(4) USAGE COMP.                        
           10 G48SPKUENDMERKMAL    PIC X(1).                                    
           10 G48SPDATVERTKUEND    PIC X(8).                                    
           10 G48SPBONIFIKATION    PIC S9(3)V9(4) USAGE COMP-3.                 
           10 G48SPURSPRVERTNR     PIC X(12).                                   
           10 G48SPPIN             PIC S9(9)V USAGE COMP-3.                     
           10 G48SPIBAN            PIC X(34).                                   
           10 G48SPBIC             PIC X(11).                                   
           10 G48SPMANDAT          PIC X(35).                                   
      ************************** ENDE-DCLGEN *************************          
