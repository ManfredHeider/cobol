      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0071.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0071.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:28   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-RECHTSFORM-GESAMT.
         10 EA-NI-RECHTSFORM-TAB.
           20 EA-NI-RECHTSFORM-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-RECHTSFORM
                 REDEFINES EA-NI-RECHTSFORM-TAB.
           20 EA-NI-RECHTSFORMID             PIC X.
           20 EA-NI-RECHTSFORMTEXT           PIC X.
         10 EA-DCL-RECHTSFORM.
           20 EA-RECHTSFORMID                PIC -----9    .
           20 EA-RECHTSFORMTEXT              PIC X(30).
      ******************************************************************
