      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CTKI0485.CPY  $                                         
      *    $Revision:   2.3  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/CTKI0485.CPv  $                                    
      *                                                                         
      *   Rev 2.3   Feb 25 2021 10:52:16   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.abc - DTA's erweitern: Sourc             
      *en Db2                                                                   
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:54   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:32:30   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
CCC005*    EXEC SQL DECLARE G20_VERTRAGSTEIL TABLE                              
CCC004*    ( G20TVNR                        CHAR(15) NOT NULL,                  
CCC004*      G20TLFDNR                      SMALLINT NOT NULL,                  
CCC004*      G20TVERARB_KZN                 CHAR(1) NOT NULL,                   
CCC004*      G20TBEZUGLFDNR                 DECIMAL(5, 0) NOT NULL,             
CCC004*      G20THVT_KZN                    CHAR(1) NOT NULL,                   
CCC004*      G20TTARIFKLASSE                DECIMAL(5, 0) NOT NULL,             
CCC004*      G20TBEGINN                     CHAR(10) NOT NULL,                  
CCC004*      G20TABLAUF                     CHAR(10) NOT NULL,                  
CCC004*      G20TBEITRZUSTID                SMALLINT NOT NULL,                  
CCC004*      G20TVERSSUMME                  DECIMAL(13, 2) NOT NULL,            
CCC004*      G20TABLBZ                      CHAR(10) NOT NULL,                  
CCC004*      G20TTARIFBEITRAG               DECIMAL(13, 2) NOT NULL,            
CCC004*      G20TVZUST_D                    SMALLINT NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G20_VERTRAGSTEIL            *               
      ******************************************************************        
       01  G20-VERTRAGSTEIL-GESAMT.                                             
        05 NI-G20-VERTRAGSTEIL-TAB.                                             
           10 NI-G20-VERTRAGSTEIL-ELE PIC S9(04) COMP OCCURS 13 TIMES.          
        05 NI-G20-VERTRAGSTEIL REDEFINES NI-G20-VERTRAGSTEIL-TAB.               
           10 NI-G20TVNR            PIC S9(4) COMP.                             
           10 NI-G20TLFDNR          PIC S9(4) COMP.                             
           10 NI-G20TVERARB-KZN     PIC S9(4) COMP.                             
           10 NI-G20TBEZUGLFDNR     PIC S9(4) COMP.                             
           10 NI-G20THVT-KZN        PIC S9(4) COMP.                             
           10 NI-G20TTARIFKLASSE    PIC S9(4) COMP.                             
           10 NI-G20TBEGINN         PIC S9(4) COMP.                             
           10 NI-G20TABLAUF         PIC S9(4) COMP.                             
           10 NI-G20TBEITRZUSTID    PIC S9(4) COMP.                             
           10 NI-G20TVERSSUMME      PIC S9(4) COMP.                             
           10 NI-G20TABLBZ          PIC S9(4) COMP.                             
           10 NI-G20TTARIFBEITRAG   PIC S9(4) COMP.                             
           10 NI-G20TVZUST-D        PIC S9(4) COMP.                             
        05 DCL-G20-VERTRAGSTEIL.                                                
           10 G20TVNR              PIC X(15).                                   
           10 G20TLFDNR            PIC S9(4) USAGE COMP.                        
           10 G20TVERARB-KZN       PIC X(1).                                    
           10 G20TBEZUGLFDNR       PIC S9(5)V USAGE COMP-3.                     
           10 G20THVT-KZN          PIC X(1).                                    
           10 G20TTARIFKLASSE      PIC S9(5)V USAGE COMP-3.                     
           10 G20TBEGINN           PIC X(10).                                   
           10 G20TABLAUF           PIC X(10).                                   
           10 G20TBEITRZUSTID      PIC S9(4) USAGE COMP.                        
           10 G20TVERSSUMME        PIC S9(11)V9(2) USAGE COMP-3.                
           10 G20TABLBZ            PIC X(10).                                   
           10 G20TTARIFBEITRAG     PIC S9(11)V9(2) USAGE COMP-3.                
           10 G20TVZUST-D          PIC S9(4) USAGE COMP.                        
      ************************** ENDE-DCLGEN *************************          
