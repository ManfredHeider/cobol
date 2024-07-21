      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0392.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0392.CPv  $
      *
      *   Rev 2.1   Oct 15 2002 08:15:56   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.0   Sep 30 2002 15:05:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-RISIKOKLASSE-GESAMT.
         10 EA-NI-RISIKOKLASSE-TAB.
           20 EA-NI-RISIKOKLASSE-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-RISIKOKLASSE
                 REDEFINES EA-NI-RISIKOKLASSE-TAB.
           20 EA-NI-RISRISIKOKLASSEID        PIC X.
           20 EA-NI-RISRISIKOKLTEXT          PIC X.
         10 EA-DCL-RISIKOKLASSE.
           20 EA-RISRISIKOKLASSEID           PIC -----9.
           20 EA-RISRISIKOKLTEXT             PIC X(30).
      *****************************************************************
