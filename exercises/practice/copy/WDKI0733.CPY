      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0733.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0733.CPv  $
      *
      *   Rev 2.1   May 27 2003 11:39:30   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   May 14 2003 15:20:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELPUBLOCINST-GESAMT.
         10 EA-NI-SELPUBLOCINST-TAB.
           20 EA-NI-SELPUBLOCINST-ELE PIC X OCCURS 4 TIMES.
         10 EA-NI-SELPUBLOCINST REDEFINES EA-NI-SELPUBLOCINST-TAB.
           20 EA-NI-SPLID                    PIC X.
           20 EA-NI-SPLTARGETVBN             PIC X.
           20 EA-NI-SPLSANID                 PIC X.
           20 EA-NI-SPLSTATUS                PIC X.
         10 EA-DCL-SELPUBLOCINST.
           20 EA-SPLID                       PIC X(13).
           20 EA-SPLTARGETVBN                PIC ----------9.
           20 EA-SPLSANID                    PIC X(13).
           20 EA-SPLSTATUS                   PIC X(1).
      *****************************************************************
