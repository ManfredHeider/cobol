      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0085.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0085.CPv  $
      *
      *   Rev 2.1   Jun 11 2003 13:08:24   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   Jun 05 2003 10:08:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-EIGENTUMMIETE.
           10 EMPERSONENID                PIC X(13).
           10 EMPERSONENROLLE             PIC X(1).
           10 EMLAUFENDENUMMER            PIC S9(4) COMP.
           10 EMTYPE                      PIC S9(4) COMP.
           10 NIL-EMTYPE                  PIC X(01).
           10 EMWOHNFLAECHE               PIC S9(4)V9(2) COMP-3.
           10 NIL-EMWOHNFLAECHE           PIC X(01).
           10 EMMIETE                     PIC S9(9)V9(2) COMP-3.
           10 NIL-EMMIETE                 PIC X(01).
           10 EMNEBENKOSTEN               PIC S9(9)V9(2) COMP-3.
           10 NIL-EMNEBENKOSTEN           PIC X(01).
           10 EMMIETFINKOSTEN             PIC S9(9)V9(2) COMP-3.
           10 NIL-EMMIETFINKOSTEN         PIC X(01).
           10 EMWERTVORHANDEN             PIC S9(9)V9(2) COMP-3.
           10 NIL-EMWERTVORHANDEN         PIC X(01).
           10 EMRESTSCHULD                PIC S9(9)V9(2) COMP-3.
           10 NIL-EMRESTSCHULD            PIC X(01).
           10 EMKAUFWUNSCH                PIC X(3).
           10 NIL-EMKAUFWUNSCH            PIC X(01).
           10 EMWERTZUKAUFEN              PIC S9(9)V9(2) COMP-3.
           10 NIL-EMWERTZUKAUFEN          PIC X(01).
           10 EMWERTGRUNDSTUECK           PIC S9(9)V9(2) COMP-3.
           10 NIL-EMWERTGRUNDSTUECK       PIC X(01).
           10 EMKAUFBAUDATUM              PIC X(010).
           10 NIL-EMKAUFBAUDATUM          PIC X(01).
      *****************************************************************
