      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0734.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0734.CPv  $
      *
      *   Rev 2.1   May 27 2003 11:39:30   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   May 14 2003 15:20:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELPUBPOOLDATA-GESAMT.
         10 EA-NI-SELPUBPOOLDATA-TAB.
           20 EA-NI-SELPUBPOOLDATA-ELE PIC X OCCURS 4 TIMES.
         10 EA-NI-SELPUBPOOLDATA REDEFINES EA-NI-SELPUBPOOLDATA-TAB.
           20 EA-NI-SPDID                    PIC X.
           20 EA-NI-SPDFIELDID               PIC X.
           20 EA-NI-SPDOPERTID               PIC X.
           20 EA-NI-SPDCRITID                PIC X.
         10 EA-DCL-SELPUBPOOLDATA.
           20 EA-SPDID                       PIC X(13).
           20 EA-SPDFIELDID                  PIC ----------9.
           20 EA-SPDOPERTID                  PIC ----------9.
           20 EA-SPDCRITID                   PIC ----------9.
      *****************************************************************
