      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0452.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0452.CPv  $
      *
      *   Rev 2.3   Nov 26 2012 15:28:42   T13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.2   Oct 19 2012 13:40:22   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:28:16   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 16:41:50   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-CKVPERSHISTORIE.
           10 PHIVERTRAGSID                 PIC  X(0013).
           10 PHIPERSONENNR                 PIC S9(4) COMP.
           10 PHIDATUMHISTORIE              PIC S9(9) COMP.
           10 PHITARIFHISTORIE              PIC  X(0012).
           10 PHITAGESSATZ                  PIC S9(7)V9(2) COMP-3.
           10 NIL-PHITAGESSATZ              PIC  X(01).
           10 PHIBEITRAG                    PIC S9(7)V9(2) COMP-3.
           10 NIL-PHIBEITRAG                PIC  X(01).
           10 PHIGESETZZUSCHLG              PIC S9(7)V9(2) COMP-3.
           10 NIL-PHIGESETZZUSCHLG          PIC  X(01).
           10 PHIRISIKOZUSCHLG              PIC S9(7)V9(2) COMP-3.
           10 NIL-PHIRISIKOZUSCHLG          PIC  X(01).
           10 PHISELBSTBEHALT               PIC S9(7)V9(2) COMP-3.
           10 NIL-PHISELBSTBEHALT           PIC  X(01).
           10 PHIGUTSCHRIFTBEF              PIC S9(7)V9(2) COMP-3.
           10 NIL-PHIGUTSCHRIFTBEF          PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
