      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WUKI0485.CPY  $                                         
      *    $Revision:   2.3  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/WUKI0485.CPv  $                                    
      *                                                                         
      *   Rev 2.3   Feb 25 2021 10:51:30   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.abc - DTA's erweitern: Sourc             
      *en Db2                                                                   
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:32   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:32:59   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
        05  WU-WUKI0485.                                                        
           10 G20TVNR                       PIC  X(0015).                       
           10 G20TLFDNR                     PIC S9(4) COMP.                     
           10 G20TVERARB-KZN                PIC  X(0001).                       
           10 G20TBEZUGLFDNR                PIC S9(5)V COMP-3.                  
           10 G20THVT-KZN                   PIC  X(0001).                       
           10 G20TTARIFKLASSE               PIC S9(5)V COMP-3.                  
           10 G20TBEGINN                    PIC  X(0010).                       
           10 G20TABLAUF                    PIC  X(0010).                       
           10 G20TBEITRZUSTID               PIC S9(4) COMP.                     
           10 G20TVERSSUMME                 PIC S9(11)V9(2) COMP-3.             
           10 G20TABLBZ                     PIC  X(0010).                       
           10 G20TTARIFBEITRAG              PIC S9(11)V9(2) COMP-3.             
           10 ZUGANG                        PIC  X(0032).                       
           10 VON                           PIC  X(0032).                       
           10 BIS                           PIC  X(0032).                       
           10 DCOP                          PIC  X(0001).                       
           10 NIL-DCOP                      PIC  X(01).                         
           10 G20TVZUST-D                   PIC S9(4) COMP.                     
