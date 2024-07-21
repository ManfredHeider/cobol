      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0619.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0619.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PCMIGRATIONMASTER-GESAMT.
         10 EA-NI-PCMIGRATIONMASTER-TAB.
           20 EA-NI-PCMIGRATIONMASTER-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-PCMIGRATIONMASTER
                 REDEFINES EA-NI-PCMIGRATIONMASTER-TAB.
           20 EA-NI-MIGRATIONID              PIC X.
           20 EA-NI-PCLINIE                  PIC X.
           20 EA-NI-VBNRNEU                  PIC X.
           20 EA-NI-PCNR                     PIC X.
           20 EA-NI-LOESCHKZ                 PIC X.
           20 EA-NI-PCLINIENEU               PIC X.
           20 EA-NI-DATUM                    PIC X.
           20 EA-NI-VBNR                     PIC X.
           20 EA-NI-PCNRNEU                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PCMIGRATIONMASTER.
           20 EA-MIGRATIONID                 PIC X(13).
           20 EA-PCLINIE                     PIC -----9    .
           20 EA-VBNRNEU                     PIC --------9 .
           20 EA-PCNR                        PIC --------9 .
           20 EA-LOESCHKZ                    PIC X(01).
           20 EA-PCLINIENEU                  PIC -----9    .
           20 EA-DATUM                       PIC X(10).
           20 EA-VBNR                        PIC --------9 .
           20 EA-PCNRNEU                     PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
