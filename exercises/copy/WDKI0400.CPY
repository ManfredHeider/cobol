      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0400.CPv  $
      *
      *   Rev 2.1   Apr 04 2005 08:39:34   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.0   Mar 03 2005 10:38:30   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-AENDERUNGAVM-GESAMT.
         10 EA-NI-AENDERUNGAVM-TAB.
           20 EA-NI-AENDERUNGAVM-ELE PIC X OCCURS 8 TIMES.
         10 EA-NI-AENDERUNGAVM
                 REDEFINES EA-NI-AENDERUNGAVM-TAB.
           20 EA-NI-AAVVERTRAGSID            PIC X.
           20 EA-NI-AAVLAUFENDENUMMER        PIC X.
           20 EA-NI-AAVABSCHLVBNEU           PIC X.
           20 EA-NI-AAVABSCHLVBNEURKZ        PIC X.
           20 EA-NI-AAVABSCHLVBALT           PIC X.
           20 EA-NI-AAVABSCHLVBALTRKZ        PIC X.
           20 EA-NI-AAVBEARBEITER            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AENDERUNGAVM.
           20 EA-AAVVERTRAGSID               PIC X(13).
           20 EA-AAVLAUFENDENUMMER           PIC -----9.
           20 EA-AAVABSCHLVBNEU              PIC X(13).
           20 EA-AAVABSCHLVBNEURKZ           PIC X(1).
           20 EA-AAVABSCHLVBALT              PIC X(13).
           20 EA-AAVABSCHLVBALTRKZ           PIC X(1).
           20 EA-AAVBEARBEITER               PIC X(8).
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
