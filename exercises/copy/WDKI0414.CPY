      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0414.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0414.CPv  $
      *
      *   Rev 2.1   Jul 24 2006 09:32:56   t13ex27
      *AKI00604: DB-Änderung für Fix 90 - EA-Programme
      *
      *   Rev 2.0   Jun 22 2006 12:27:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-DBUMSATZ-GESAMT.
         10 EA-NI-DBUMSATZ-TAB.
           20 EA-NI-DBUMSATZ-ELE PIC X OCCURS 8 TIMES.
         10 EA-NI-DBUMSATZ
                 REDEFINES EA-NI-DBUMSATZ-TAB.
           20 EA-NI-DBUVERTRAGSID            PIC X.
           20 EA-NI-DBUGHPLFN                PIC X.
           20 EA-NI-DBULFDNR                 PIC X.
           20 EA-NI-DBUJAHR                  PIC X.
           20 EA-NI-DBUMONAT                 PIC X.
           20 EA-NI-DBUHABEN                 PIC X.
           20 EA-NI-DBUSOLL                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DBUMSATZ.
           20 EA-DBUVERTRAGSID               PIC X(13).
           20 EA-DBUGHPLFN                   PIC -----9.
           20 EA-DBULFDNR                    PIC -----9.
           20 EA-DBUJAHR                     PIC -----9.
           20 EA-DBUMONAT                    PIC -----9.
           20 EA-DBUHABEN                    PIC ------------9.99.
           20 EA-DBUSOLL                     PIC ------------9.99.
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
