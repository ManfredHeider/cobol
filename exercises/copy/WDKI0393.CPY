      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0393.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0393.CPv  $
      *
      *   Rev 2.1   Oct 15 2002 08:15:56   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.0   Sep 30 2002 15:05:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-MISCHFOND-GESAMT.
         10 EA-NI-MISCHFOND-TAB.
           20 EA-NI-MISCHFOND-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-MISCHFOND
                 REDEFINES EA-NI-MISCHFOND-TAB.
           20 EA-NI-MISCHFONDID              PIC X.
           20 EA-NI-MISCHAKTIEN              PIC X.
           20 EA-NI-MISCHRENTEN              PIC X.
           20 EA-NI-MISCHIMMO                PIC X.
           20 EA-NI-MISCHGELD                PIC X.
           20 EA-NI-MISCHSONST               PIC X.
         10 EA-DCL-MISCHFOND.
           20 EA-MISCHFONDID                 PIC -----9.
           20 EA-MISCHAKTIEN                 PIC -9.99.
           20 EA-MISCHRENTEN                 PIC -9.99.
           20 EA-MISCHIMMO                   PIC -9.99.
           20 EA-MISCHGELD                   PIC -9.99.
           20 EA-MISCHSONST                  PIC -9.99.
      *****************************************************************
