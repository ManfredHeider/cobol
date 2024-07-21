      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0610.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0610.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:20   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HARDWARE-SETTINGS-GESAMT.
         10 EA-NI-HARDWARE-SETTINGS-TAB.
           20 EA-NI-HARDWARE-SETTINGS-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-HARDWARE-SETTINGS
                 REDEFINES EA-NI-HARDWARE-SETTINGS-TAB.
           20 EA-NI-HWSWORKSTATIONID         PIC X.
           20 EA-NI-HWSBETRIEBSSYSTEM        PIC X.
           20 EA-NI-HWSPARAMETERNAME         PIC X.
           20 EA-NI-HWSPARAMETERWERT         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-HARDWARE-SETTINGS.
           20 EA-HWSWORKSTATIONID            PIC --------9 .
           20 EA-HWSBETRIEBSSYSTEM           PIC X(5).
           20 EA-HWSPARAMETERNAME.
              49 EA-HWSPARAMETERNAME-LEN     PIC -----9    .
              49 EA-HWSPARAMETERNAME-TEXT    PIC X(200).
           20 EA-HWSPARAMETERWERT.
              49 EA-HWSPARAMETERWERT-LEN     PIC -----9    .
              49 EA-HWSPARAMETERWERT-TEXT    PIC X(200).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
