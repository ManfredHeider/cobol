      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0348.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0348.CPv  $
      *
      *   Rev 2.1   16 Dec 2008 11:29:40   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.0   11 Dec 2008 10:14:08   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0348.
           10 VETVERTRAGSID               PIC  X(13).
           10 VETVERSTARLFDNR             PIC  S9(4) COMP.
           10 VETPRODUKTID                PIC  S9(4) COMP.
           10 NIL-VETPRODUKTID            PIC X(01).
           10 VETVERSSUMME                PIC  S9(9)V9(2) COMP-3.
           10 NIL-VETVERSSUMME            PIC X(01).
           10 VETVERSSUMMEWKZ             PIC  X(1).
           10 VETSUBTYPE                  PIC  X(1).
           10 NIL-VETSUBTYPE              PIC X(01).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 VETVERSSUMMEBKZ             PIC  X(1).
           10 NIL-VETVERSSUMMEBKZ         PIC X(01).
