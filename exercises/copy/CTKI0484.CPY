      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  16/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CTKI0484.CPY  $                                         
      *    $Revision:   2.3  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/CTKI0484.CPv  $                                    
      *                                                                         
      *   Rev 2.3   Jan 19 2021 08:50:32   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.a - allgemeine Vertragsdaten             
      * (LBNPSVERTRAG) erweitern: Sourcen Db2                                   
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:50   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 16 2020 14:20:32   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
CCC005*    EXEC SQL DECLARE G20_VERTRAG TABLE                                   
CCC004*    ( G20VVNR                        CHAR(15) NOT NULL,                  
CCC004*      G20VMANDANT                    INTEGER NOT NULL,                   
CCC004*      G20VVERARB_KZN                 CHAR(1) NOT NULL,                   
CCC004*      G20VVERTRIEBSTAT               CHAR(1) NOT NULL,                   
CCC004*      G20VINFSPERR_KZN               CHAR(1) NOT NULL,                   
CCC004*      G20VMAKLER_KZN                 CHAR(1) NOT NULL,                   
CCC004*      G20VVBNR                       CHAR(7) NOT NULL,                   
CCC004*      G20VZAHLWID                    SMALLINT NOT NULL,                  
CCC004*      G20VPRODKLASSE                 DECIMAL(15, 0) NOT NULL,            
CCC004*      G20VGSMTTAR_BEZ                CHAR(25) NOT NULL,                  
CCC004*      G20VWERBEBEZEICH               DECIMAL(15, 0) NOT NULL,            
CCC004*      G20VRNTBEZUG_KZN               CHAR(1) NOT NULL,                   
CCC004*      G20VVZUST_G                    SMALLINT NOT NULL,                  
CCC004*      G20VVZUST_D                    SMALLINT NOT NULL,                  
CCC004*      G20VBAV_KZN                    CHAR(1) NOT NULL,                   
CCC004*      G20VSTORNODAT                  CHAR(10) NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G20_VERTRAG                 *               
      ******************************************************************        
       01  G20-VERTRAG-GESAMT.                                                  
        05 NI-G20-VERTRAG-TAB.                                                  
           10 NI-G20-VERTRAG-ELE PIC S9(04) COMP OCCURS 16 TIMES.               
        05 NI-G20-VERTRAG REDEFINES NI-G20-VERTRAG-TAB.                         
           10 NI-G20VVNR            PIC S9(4) COMP.                             
           10 NI-G20VMANDANT        PIC S9(4) COMP.                             
           10 NI-G20VVERARB-KZN     PIC S9(4) COMP.                             
           10 NI-G20VVERTRIEBSTAT   PIC S9(4) COMP.                             
           10 NI-G20VINFSPERR-KZN   PIC S9(4) COMP.                             
           10 NI-G20VMAKLER-KZN     PIC S9(4) COMP.                             
           10 NI-G20VVBNR           PIC S9(4) COMP.                             
           10 NI-G20VZAHLWID        PIC S9(4) COMP.                             
           10 NI-G20VPRODKLASSE     PIC S9(4) COMP.                             
           10 NI-G20VGSMTTAR-BEZ    PIC S9(4) COMP.                             
           10 NI-G20VWERBEBEZEICH   PIC S9(4) COMP.                             
           10 NI-G20VRNTBEZUG-KZN   PIC S9(4) COMP.                             
           10 NI-G20VVZUST-G        PIC S9(4) COMP.                             
           10 NI-G20VVZUST-D        PIC S9(4) COMP.                             
           10 NI-G20VBAV-KZN        PIC S9(4) COMP.                             
           10 NI-G20VSTORNODAT      PIC S9(4) COMP.                             
        05 DCL-G20-VERTRAG.                                                     
           10 G20VVNR              PIC X(15).                                   
           10 G20VMANDANT          PIC S9(9) USAGE COMP.                        
           10 G20VVERARB-KZN       PIC X(1).                                    
           10 G20VVERTRIEBSTAT     PIC X(1).                                    
           10 G20VINFSPERR-KZN     PIC X(1).                                    
           10 G20VMAKLER-KZN       PIC X(1).                                    
           10 G20VVBNR             PIC X(7).                                    
           10 G20VZAHLWID          PIC S9(4) USAGE COMP.                        
           10 G20VPRODKLASSE       PIC S9(15)V USAGE COMP-3.                    
           10 G20VGSMTTAR-BEZ      PIC X(25).                                   
           10 G20VWERBEBEZEICH     PIC S9(15)V USAGE COMP-3.                    
           10 G20VRNTBEZUG-KZN     PIC X(1).                                    
           10 G20VVZUST-G          PIC S9(4) USAGE COMP.                        
           10 G20VVZUST-D          PIC S9(4) USAGE COMP.                        
           10 G20VBAV-KZN          PIC X(1).                                    
           10 G20VSTORNODAT        PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
