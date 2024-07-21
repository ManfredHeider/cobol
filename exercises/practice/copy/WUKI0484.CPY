      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  16/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WUKI0484.CPY  $                                         
      *    $Revision:   2.3  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/WUKI0484.CPv  $                                    
      *                                                                         
      *   Rev 2.3   Jan 19 2021 08:49:36   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.a - allgemeine Vertragsdaten             
      * (LBNPSVERTRAG) erweitern: Sourcen Db2                                   
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:28   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 16 2020 14:20:36   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
        05  WU-WUKI0484.                                                        
           10 G20VVNR                       PIC  X(0015).                       
           10 G20VMANDANT                   PIC S9(9) COMP.                     
           10 G20VVERARB-KZN                PIC  X(0001).                       
           10 G20VVERTRIEBSTAT              PIC  X(0001).                       
           10 G20VINFSPERR-KZN              PIC  X(0001).                       
           10 G20VMAKLER-KZN                PIC  X(0001).                       
           10 G20VVBNR                      PIC  X(0007).                       
           10 G20VZAHLWID                   PIC S9(4) COMP.                     
           10 G20VPRODKLASSE                PIC S9(15)V COMP-3.                 
           10 G20VGSMTTAR-BEZ               PIC  X(0025).                       
           10 G20VWERBEBEZEICH              PIC S9(15)V COMP-3.                 
           10 G20VRNTBEZUG-KZN              PIC  X(0001).                       
           10 ZUGANG                        PIC  X(0032).                       
           10 VON                           PIC  X(0032).                       
           10 BIS                           PIC  X(0032).                       
           10 DCOP                          PIC  X(0001).                       
           10 NIL-DCOP                      PIC  X(01).                         
           10 G20VVZUST-G                   PIC S9(4) COMP.                     
           10 G20VVZUST-D                   PIC S9(4) COMP.                     
           10 G20VBAV-KZN                   PIC  X(0001).                       
           10 G20VSTORNODAT                 PIC  X(0010).                       
