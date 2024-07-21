      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0372.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0372.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:26   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:52   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NBPROZESS-GESAMT.
         10 EA-NI-NBPROZESS-TAB.
           20 EA-NI-NBPROZESS-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-NBPROZESS
                 REDEFINES EA-NI-NBPROZESS-TAB.
           20 EA-NI-NBSEITENPROZESS          PIC X.
           20 EA-NI-NBPRODUKT                PIC X.
           20 EA-NI-FREMDVERTRAGSKZ          PIC X.
           20 EA-NI-POSITION                 PIC X.
         10 EA-DCL-NBPROZESS.
           20 EA-NBSEITENPROZESS             PIC -----9    .
           20 EA-NBPRODUKT                   PIC -----9    .
           20 EA-FREMDVERTRAGSKZ             PIC X(01).
           20 EA-POSITION                    PIC -----9    .
      ******************************************************************
