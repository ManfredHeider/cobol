      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0329.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/WUKI0329.CPv  $
      *
      *   Rev 2.9   Sep 20 2017 15:20:28   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.8   16 Dec 2008 11:29:32   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.7   Dec 12 2005 09:20:00   t13ex27
      *AKI00533: DB-Änderung für Fix 82 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jun 12 2003 11:15:46   t13ex27
      *AKI00351 - DB-Änderung Korrektur
      *
      *   Rev 2.5   25 Jul 2001 10:33:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0329.
           10 LTLVERTRAGSID                 PIC  X(0013).
           10 LTLVERSTARLFDNR               PIC S9(4) COMP.
           10 LTLLAUFENDENR                 PIC S9(4) COMP.
           10 LTLAUSZAHLBETRAG              PIC S9(9)V9(2) COMP-3.
           10 NIL-LTLAUSZAHLBETRAG          PIC  X(01).
           10 LTLAUSZAHLZEITPKT             PIC  X(010).
           10 NIL-LTLAUSZAHLZEITPKT         PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 LTLVERRENTKAPBETR             PIC S9(9)V9(2) COMP-3.
           10 NIL-LTLVERRENTKAPBETR         PIC  X(01).
           10 LTLVERRENTKAPBWKZ             PIC  X(0001).
           10 LTLRENTEGARZEIT               PIC S9(4) COMP.
           10 NIL-LTLRENTEGARZEIT           PIC  X(01).
           10 LTLAUSZDECKKAPBETR            PIC S9(9)V9(2) COMP-3.
           10 NIL-LTLAUSZDECKKAPBETR        PIC  X(01).
           10 LTLAUSZBETRAGBKZ              PIC  X(0001).
           10 NIL-LTLAUSZBETRAGBKZ          PIC  X(01).
           10 LTLZAHLZEITPBKZ               PIC  X(0001).
           10 NIL-LTLZAHLZEITPBKZ           PIC  X(01).
           10 LTLVERRENTKAPBKZ              PIC  X(0001).
           10 NIL-LTLVERRENTKAPBKZ          PIC  X(01).

