      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0414.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0414.CPv  $
      *
      *   Rev 2.1   Jul 24 2006 09:47:54   t13ex27
      *AKI00604: DB-Änderung für Fix 90 - PMKI0079
      *
      *   Rev 2.0   Jun 22 2006 12:31:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-DBUMSATZ.
           10 DBUVERTRAGSID               PIC X(13).
           10 DBUGHPLFN                   PIC S9(4) COMP.
           10 DBULFDNR                    PIC S9(4) COMP.
           10 DBUJAHR                     PIC S9(4) COMP.
           10 DBUMONAT                    PIC S9(4) COMP.
           10 DBUHABEN                    PIC S9(12)V9(2) COMP-3.
           10 NIL-DBUHABEN                PIC X(01).
           10 DBUSOLL                     PIC S9(12)V9(2) COMP-3.
           10 NIL-DBUSOLL                 PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
      *****************************************************************
