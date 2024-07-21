      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0092.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0092.CPv  $
      *
      *   Rev 2.2   Oct 17 2005 10:06:56   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.1   Jun 11 2003 13:09:00   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   Jun 05 2003 10:09:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-EINKOMMENRV.
           10 EKRVPERSONENID              PIC X(13).
           10 EKRVPERSONENROLLE           PIC X(1).
           10 EKRVFREIWILLRENT            PIC S9(4) COMP.
           10 NIL-EKRVFREIWILLRENT        PIC X(01).
           10 EKRVGESRENTENJAHR           PIC S9(4) COMP.
           10 NIL-EKRVGESRENTENJAHR       PIC X(01).
           10 EKRVVERSNUMMER              PIC X(12).
           10 NIL-EKRVVERSNUMMER          PIC X(01).
           10 EKRVBERUFVERSJAHR           PIC S9(4) COMP.
           10 NIL-EKRVBERUFVERSJAHR       PIC X(01).
           10 EKRVALTERSRENTE             PIC S9(9)V9(2) COMP-3.
           10 NIL-EKRVALTERSRENTE         PIC X(01).
           10 EKRVALTERSRENTEWKZ          PIC X(1).
           10 EKRVEMRENTE                 PIC S9(9)V9(2) COMP-3.
           10 NIL-EKRVEMRENTE             PIC X(01).
           10 EKRVEMRENTEWKZ              PIC X(1).
           10 NIL-EKRVEMRENTEWKZ          PIC X(01).
      *****************************************************************
