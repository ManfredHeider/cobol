      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WUKI0384.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/WUKI0384.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Dec 11 2020 13:17:40   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Dec 02 2020 15:26:04   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:22   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:54:09   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
        05  WU-WUKI0384.                                                        
           10 LPRVERTRAGSID                 PIC  X(0013).                       
           10 LETZTEAENDERUNG               PIC  X(0026).                       
           10 LPRBERECH-KZN                 PIC  X(0001).                       
           10 LPRZAHLBTG                    PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRZAHLBTG                PIC  X(01).                         
           10 ZUGANG                        PIC  X(0032).                       
           10 VON                           PIC  X(0032).                       
           10 BIS                           PIC  X(0032).                       
           10 DCOP                          PIC  X(0001).                       
           10 NIL-DCOP                      PIC  X(01).                         
           10 LPRSUMDK                      PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRSUMDK                  PIC  X(01).                         
           10 LPRBEITRAG-VUE                PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRBEITRAG-VUE            PIC  X(01).                         
           10 LPRSUMME-VUE                  PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRSUMME-VUE              PIC  X(01).                         
           10 LPRGUTHABEN-VUE               PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRGUTHABEN-VUE           PIC  X(01).                         
           10 LPRJAHRBTG                    PIC S9(11)V9(2) COMP-3.             
           10 NIL-LPRJAHRBTG                PIC  X(01).                         
