      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0447.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0447.CPv  $
      *
      *   Rev 2.2   Jul 12 2011 08:38:06   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.1   Jul 11 2011 16:18:04   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 10 2011 17:10:50   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
        05  WU-DWSZULAGE.
           10 ZULVERTRAGSID                 PIC  X(0013).
           10 ZULGUTHABPRODLFD              PIC S9(4) COMP.
           10 ZULFONDID                     PIC S9(4) COMP.
           10 ZULFOZLFDNR                   PIC S9(4) COMP.
           10 ZULLFDNR                      PIC S9(4) COMP.
           10 ZULPORTFOLIONR                PIC  X(0010).
           10 NIL-ZULPORTFOLIONR            PIC  X(01).
           10 ZULBEITRJAHR                  PIC S9(4) COMP.
           10 NIL-ZULBEITRJAHR              PIC  X(01).
           10 ZULSUMBEITRJAHR               PIC S9(13)V9(2) COMP-3.
           10 NIL-ZULSUMBEITRJAHR           PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
