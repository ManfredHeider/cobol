      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21/09/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   WUKI0445.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/WUKI0445.CPv  $                                  
      *
      *   Rev 2.1   Sep 29 2010 10:51:12   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 21 2010 14:42:52   T13EX14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
        05  WU-SACHWIEN.                                                
           10 SWIVERTRAGSID                 PIC  X(0013).               
           10 SWIVEZLFDNR                   PIC S9(4) COMP.             
           10 SWIHOSTID                     PIC  X(0015).               
           10 SWIUNFSUMME                   PIC S9(10)V COMP-3.         
           10 NIL-SWIUNFSUMME               PIC  X(01).                 
           10 SWIRENTENART-KTY              PIC S9(4) COMP.             
           10 NIL-SWIRENTENART-KTY          PIC  X(01).                 
           10 SWIRENTENART-KNR              PIC S9(15)V COMP-3.         
           10 NIL-SWIRENTENART-KNR          PIC  X(01).                 
           10 SWIUNFKAPINVPROZ              PIC S9(3)V COMP-3.          
           10 NIL-SWIUNFKAPINVPROZ          PIC  X(01).                 
           10 SWIINVSUMME                   PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWIINVSUMME               PIC  X(01).                 
           10 SWIUNFALLTODPR                PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWIUNFALLTODPR            PIC  X(01).                 
           10 SWIVERSSUMME                  PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWIVERSSUMME              PIC  X(01).                 
           10 SWITAGEGELD                   PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWITAGEGELD               PIC  X(01).                 
           10 SWITAGEGELDTT                 PIC S9(4) COMP.             
           10 NIL-SWITAGEGELDTT             PIC  X(01).                 
           10 SWIUNFKAPVAR-KTY              PIC S9(4) COMP.             
           10 NIL-SWIUNFKAPVAR-KTY          PIC  X(01).                 
           10 SWIUNFKAPVAR-KNR              PIC S9(15)V COMP-3.         
           10 NIL-SWIUNFKAPVAR-KNR          PIC  X(01).                 
           10 SWIKFZKZ                      PIC  X(0012).               
           10 NIL-SWIKFZKZ                  PIC  X(01).                 
           10 SWIGESAMTBEITRAG              PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWIGESAMTBEITRAG          PIC  X(01).                 
           10 SWIRATENBEITRAG               PIC S9(11)V9(2) COMP-3.     
           10 NIL-SWIRATENBEITRAG           PIC  X(01).                 
           10 LETZTEAENDERUNG               PIC  X(026).                
           10 NIL-LETZTEAENDERUNG           PIC  X(01).                 
           10 SWIBUENDLBEZ-KTY              PIC S9(4) COMP.             
           10 NIL-SWIBUENDLBEZ-KTY          PIC  X(01).                 
           10 SWIBUENDLBEZ-KNR              PIC S9(15)V COMP-3.         
           10 NIL-SWIBUENDLBEZ-KNR          PIC  X(01).                 
