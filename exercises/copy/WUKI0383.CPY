      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WUKI0383.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/WUKI0383.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Dec 11 2020 13:16:52   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Dec 02 2020 15:25:14   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:18   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 18:04:15   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
        05  WU-WUKI0383.                                                        
           10 LPTVERTRAGSID                 PIC  X(0013).                       
           10 LPTLFDNR                      PIC S9(4) COMP.                     
           10 LETZTEAENDERUNG               PIC  X(0026).                       
           10 LPTBEZUGLFDNR                 PIC S9(4) COMP.                     
           10 LPTHVT-KZN                    PIC  X(0001).                       
           10 LPTTARKLASSE-KTY              PIC S9(4) COMP.                     
           10 LPTTARKLASSE-KNR              PIC S9(15)V COMP-3.                 
           10 LPTBEGINN                     PIC  X(010).                        
           10 NIL-LPTBEGINN                 PIC  X(01).                         
           10 LPTABLAUF                     PIC  X(010).                        
           10 NIL-LPTABLAUF                 PIC  X(01).                         
           10 LPTBEITRZUST-KTY              PIC S9(4) COMP.                     
           10 LPTBEITRZUST-KNR              PIC S9(15)V COMP-3.                 
           10 LPTABLBZ                      PIC  X(010).                        
           10 NIL-LPTABLBZ                  PIC  X(01).                         
           10 LPTTARIFBEITRAG               PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPTTARIFBEITRAG           PIC  X(01).                         
           10 ZUGANG                        PIC  X(0032).                       
           10 VON                           PIC  X(0032).                       
           10 BIS                           PIC  X(0032).                       
           10 DCOP                          PIC  X(0001).                       
           10 NIL-DCOP                      PIC  X(01).                         
           10 LPTRTBTGVT                    PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPTRTBTGVT                PIC  X(01).                         
           10 LPTZAHLBTGVT                  PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPTZAHLBTGVT              PIC  X(01).                         
           10 LPTVERSSUMME                  PIC S9(11)V9(2) COMP-3.             
           10 LPTVZUST-D-KTY                PIC S9(4) COMP.                     
           10 LPTVZUST-D-KNR                PIC S9(4) COMP.                     
           10 LPTVZUST-D-DAT                PIC  X(010).                        
           10 NIL-LPTVZUST-D-DAT            PIC  X(01).                         
