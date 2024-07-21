      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0345.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0345.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:12:06   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERSKRANKENTARIF.
           10 KRTVERTRAGSID               PIC X(13).
           10 KRTVERSTARLFDNR             PIC S9(4) COMP.
           10 KRTPERSONENNUMMER           PIC S9(4) COMP.
           10 NIL-KRTPERSONENNUMMER       PIC X(01).
           10 KRTBEITRAGVERZ              PIC S9(2)V9(1) COMP-3.
           10 NIL-KRTBEITRAGVERZ          PIC X(01).
           10 KRTGESCHAEFTSSTID           PIC S9(4) COMP.
           10 NIL-KRTGESCHAEFTSSTID       PIC X(01).
           10 KRTGESCHAEFTSST             PIC X(20).
           10 NIL-KRTGESCHAEFTSST         PIC X(01).
           10 KRTBEITRFRWILLWILL          PIC S9(9)V9(2) COMP-3.
           10 NIL-KRTBEITRFRWILLWILL      PIC X(01).
           10 KRTBEITRAGPRIVAT            PIC S9(9)V9(2) COMP-3.
           10 NIL-KRTBEITRAGPRIVAT        PIC X(01).
      *****************************************************************
