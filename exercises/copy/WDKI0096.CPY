      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0096.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0096.CPv  $
      *
      *   Rev 2.1   Oct 15 2002 08:15:46   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.0   Sep 30 2002 12:42:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-KKVBVORSCHLAG-GESAMT.
         10 EA-NI-KKVBVORSCHLAG-TAB.
           20 EA-NI-KKVBVORSCHLAG-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-KKVBVORSCHLAG
                 REDEFINES EA-NI-KKVBVORSCHLAG-TAB.
           20 EA-NI-KKVBVLID,                PIC X.
           20 EA-NI-KKVBVORLFDNR,            PIC X.
           20 EA-NI-KKVBEMPFVBNR,            PIC X.
           20 EA-NI-KKVBLFDNR,               PIC X.
           20 EA-NI-KKVBVBID,                PIC X.
           20 EA-NI-LETZTEAENDERUNG,         PIC X.
         10 EA-DCL-KKVBVORSCHLAG.
           20 EA-KKVBVLID                    PIC X(13).
           20 EA-KKVBVORLFDNR                PIC -----9.
           20 EA-KKVBEMPFVBNR                PIC --------9.
           20 EA-KKVBLFDNR                   PIC -----9.
           20 EA-KKVBVBID                    PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
