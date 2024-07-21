      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0436.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0436.CPv  $
      *
      *   Rev 2.2   Nov 21 2013 10:14:00   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   10 Jul 2008 10:58:26   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.0   09 Jul 2008 11:09:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-KFZSONDAUSSTATT.
           10 KFZSONVERTRAGSID              PIC  X(0013).
           10 KFZSONLFDNR                   PIC S9(4) COMP.
           10 KFZSONTEILNR-KTY              PIC S9(4) COMP.
           10 KFZSONTEILNR-KNR              PIC S9(15)V COMP-3.
           10 KFZSONTEILEWERT               PIC S9(7)V9(2) COMP-3.
           10 NIL-KFZSONTEILEWERT           PIC  X(01).
