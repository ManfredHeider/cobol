      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0709.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0709.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:30:18   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:10   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-STRUKTURART-GESAMT.
         10 EA-NI-STRUKTURART-TAB.
           20 EA-NI-STRUKTURART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-STRUKTURART
                 REDEFINES EA-NI-STRUKTURART-TAB.
           20 EA-NI-STRUKTURARTID            PIC X.
           20 EA-NI-STRUKTURARTTEXT          PIC X.
         10 EA-DCL-STRUKTURART.
           20 EA-STRUKTURARTID               PIC -----9    .
           20 EA-STRUKTURARTTEXT             PIC X(50).
      ******************************************************************
