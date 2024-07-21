      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26/06/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUAT0034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUAT0034.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:27:28   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.1   Jul 07 2015 11:14:04   t13ex27
      *AAT00049: Fehlerverfahren mit Prozentkriterien-OZPs
      *
      *   Rev 2.0   Jun 26 2015 13:02:40   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUAT0034.
           10 FAB-SYSCODE                   PIC  X(0002).
           10 FAB-JOBNAME                   PIC  X(0008).
           10 FAB-PROGRAMMNAME              PIC  X(0008).
           10 FAB-RELEVANZ                  PIC  X(0001).
           10 FAB-ANZAHL                    PIC S9(4) COMP.
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 FAB-USERID                    PIC  X(0007).
           10 FAB-PROZENT                   PIC S9(2)V9(2) COMP-3.
           10 FAB-GRENZE                    PIC S9(9) COMP.
           10 NIL-FAB-GRENZE                PIC  X(01).
           10 FAB-FAKTOR                    PIC S9(4) COMP.
           10 NIL-FAB-FAKTOR                PIC  X(01).

