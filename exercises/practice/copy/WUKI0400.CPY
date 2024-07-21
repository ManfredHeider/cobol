      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0400.CPv  $
      *
      *   Rev 2.1   Apr 04 2005 08:40:24   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.0   Mar 03 2005 10:39:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-AENDERUNGAVM.
           10 AAVVERTRAGSID               PIC X(13).
           10 AAVLAUFENDENUMMER           PIC S9(4) COMP.
           10 AAVABSCHLVBNEU              PIC X(13).
           10 AAVABSCHLVBNEURKZ           PIC X(1).
           10 AAVABSCHLVBALT              PIC X(13).
           10 NIL-AAVABSCHLVBALT          PIC X(01).
           10 AAVABSCHLVBALTRKZ           PIC X(1).
           10 NIL-AAVABSCHLVBALTRKZ       PIC X(01).
           10 AAVBEARBEITER               PIC X(8).
           10 NIL-AAVBEARBEITER           PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
      *****************************************************************
