      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0440.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0440.CPv  $
      *
      *   Rev 2.1   Sep 11 2009 07:56:24   t13ex14
      *AKI00843: DB-Änderungen für Rel. 4.60 - Badenia
      *
      *   Rev 2.0   Aug 14 2009 10:43:52   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BAUSPARJAHRWERTE.
           10 BAJVERTRAGSID               PIC  X(13).
           10 BAJGUTHPRODLFDNR            PIC  S9(4) COMP.
           10 BAJJAHR                     PIC  S9(4) COMP.
           10 BAJVLSUMJAHR                PIC  S9(7)V9(2) COMP-3.
           10 NIL-BAJVLSUMJAHR            PIC X(01).
           10 BAJWOPAVORJHR               PIC  S9(4) COMP.
           10 NIL-BAJWOPAVORJHR           PIC X(01).
           10 BAJWOPAVORVORJHR            PIC  S9(4) COMP.
           10 NIL-BAJWOPAVORVORJHR        PIC X(01).
           10 BAJSUMFOEREINZHL            PIC  S9(7)V9(2) COMP-3.
           10 NIL-BAJSUMFOEREINZHL        PIC X(01).
           10 BAJSALDWHNFOEKTO            PIC  S9(7)V9(2) COMP-3.
           10 NIL-BAJSALDWHNFOEKTO        PIC X(01).
           10 BAJSUMAVWLEINZHL            PIC  S9(7)V9(2) COMP-3.
           10 NIL-BAJSUMAVWLEINZHL        PIC X(01).
           10 BAJSUMFOERVORJHR            PIC  S9(7)V9(2) COMP-3.
           10 NIL-BAJSUMFOERVORJHR        PIC X(01).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
