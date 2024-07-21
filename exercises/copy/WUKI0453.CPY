      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0453.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WUKI0453.CPv  $
      *
      *   Rev 2.5   Aug 21 2013 16:55:08   t13ex52
      *AKI01226: G50 CENTRAL BAP Tarifumstellung 2013
      *
      *   Rev 2.3   Nov 26 2012 15:28:46   T13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.2   Oct 19 2012 13:40:28   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:28:22   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 16:41:44   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-CKVPERSUMSTTARIF.
           10 PUMVERTRAGSID                 PIC  X(0013).
           10 PUMPERSONENNR                 PIC S9(4) COMP.
           10 PUMTARIFUMSTELLG              PIC  X(0012).
           10 PUMBEITRAG                    PIC S9(7)V9(2) COMP-3.
           10 NIL-PUMBEITRAG                PIC  X(01).
           10 PUMRISIKOZUSCHLG              PIC S9(7)V9(2) COMP-3.
           10 NIL-PUMRISIKOZUSCHLG          PIC  X(01).
           10 PUMGUTSCHRIFTBEF              PIC S9(7)V9(2) COMP-3.
           10 NIL-PUMGUTSCHRIFTBEF          PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 PUMSELBSTBEHALT               PIC S9(7)V9(2) COMP-3.
           10 NIL-PUMSELBSTBEHALT           PIC  X(01).
