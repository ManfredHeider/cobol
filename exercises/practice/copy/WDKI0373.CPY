      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0373.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0373.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:34   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:52   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NBSEITENPROZESS-GESAMT.
         10 EA-NI-NBSEITENPROZESS-TAB.
           20 EA-NI-NBSEITENPROZESS-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-NBSEITENPROZESS
                 REDEFINES EA-NI-NBSEITENPROZESS-TAB.
           20 EA-NI-NBSEITENPROZESSID        PIC X.
           20 EA-NI-NBPROZESSKLASSE          PIC X.
           20 EA-NI-NBVIEWKLASSE             PIC X.
           20 EA-NI-NBSEITENTITEL            PIC X.
         10 EA-DCL-NBSEITENPROZESS.
           20 EA-NBSEITENPROZESSID           PIC -----9    .
           20 EA-NBPROZESSKLASSE             PIC X(72).
           20 EA-NBVIEWKLASSE                PIC X(72).
           20 EA-NBSEITENTITEL               PIC X(72).
      ******************************************************************
