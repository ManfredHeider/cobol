      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0613.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0613.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PROZESS-VORGABEN-GESAMT.
         10 EA-NI-PROZESS-VORGABEN-TAB.
           20 EA-NI-PROZESS-VORGABEN-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-PROZESS-VORGABEN
                 REDEFINES EA-NI-PROZESS-VORGABEN-TAB.
           20 EA-NI-PRVBENUTZERID            PIC X.
           20 EA-NI-PRVBENUTZERROLLE         PIC X.
           20 EA-NI-PRVPROZESSNAME           PIC X.
           20 EA-NI-PRVCHECKPOINT            PIC X.
           20 EA-NI-PRVINVOLVOBJEKTE         PIC X.
           20 EA-NI-PRVFENSTERMITFOCUS       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PROZESS-VORGABEN.
           20 EA-PRVBENUTZERID               PIC X(13).
           20 EA-PRVBENUTZERROLLE            PIC X(01).
           20 EA-PRVPROZESSNAME              PIC X(10).
           20 EA-PRVCHECKPOINT.
              49 EA-PRVCHECKPOINT-LEN        PIC -----9    .
              49 EA-PRVCHECKPOINT-TEXT       PIC X(200).
           20 EA-PRVINVOLVOBJEKTE.
              49 EA-PRVINVOLVOBJEKTE-LEN     PIC -----9    .
              49 EA-PRVINVOLVOBJEKTE-TEXT    PIC X(2000).
           20 EA-PRVFENSTERMITFOCUS.
              49 EA-PRVFENSTERMITFOCUS-LEN   PIC -----9    .
              49 EA-PRVFENSTERMITFOCUS-TEXT  PIC X(200).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
