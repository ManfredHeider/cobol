      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0322.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0322.CPv  $
      *
      *   Rev 2.2   Jan 25 2012 11:43:44   t13ex14
      *FKI01362: Korrektur KFZKASKOART SST-Generierung zu ZTKI0322
      *
      *   Rev 2.1   24 Sep 2008 08:43:46   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.0   24 Sep 2008 08:35:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-KFZKASKO.
           10 KFKVERTRAGSID                 PIC  X(0013).
           10 KFKVERSTARLFDNR               PIC S9(4) COMP.
           10 KFKKASKOART                   PIC  X(0002).
           10 KFKKASKOBEITRAG               PIC S9(9)V9(2) COMP-3.
           10 NIL-KFKKASKOBEITRAG           PIC  X(01).
           10 KFKKASKOBEITRWKZ              PIC  X(0001).
           10 KFKBEITRSATZFZINPR            PIC S9(3)V9(2) COMP-3.
           10 NIL-KFKBEITRSATZFZINPR        PIC  X(01).
           10 KFKREGIONALKLASSE             PIC  X(0003).
           10 NIL-KFKREGIONALKLASSE         PIC  X(01).
           10 KFKSCHADFREIHEITID            PIC S9(4) COMP.
           10 NIL-KFKSCHADFREIHEITID        PIC  X(01).
           10 KFKSEBLSTBETEILTK             PIC S9(9)V9(2) COMP-3.
           10 NIL-KFKSEBLSTBETEILTK         PIC  X(01).
           10 KFKSBTEILKASKOWKZ             PIC  X(0001).
           10 KFKSELBSTBETEILVK             PIC S9(9)V9(2) COMP-3.
           10 NIL-KFKSELBSTBETEILVK         PIC  X(01).
           10 KFKSBVOLLKASKOWKZ             PIC  X(0001).
           10 KFKSELBSTBETEILKZ             PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
