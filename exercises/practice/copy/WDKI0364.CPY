      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0364.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0364.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:26:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:48   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-WAGNIS-GESAMT.
         10 EA-NI-WAGNIS-TAB.
           20 EA-NI-WAGNIS-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-WAGNIS
                 REDEFINES EA-NI-WAGNIS-TAB.
           20 EA-NI-WAGNISID                 PIC X.
           20 EA-NI-WAGNISTEXT               PIC X.
         10 EA-DCL-WAGNIS.
           20 EA-WAGNISID                    PIC --------9 .
           20 EA-WAGNISTEXT                  PIC X(30).
      ******************************************************************
