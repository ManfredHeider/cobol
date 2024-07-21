      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0078.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:00   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ADRESSAENDGRUND-GESAMT.
         10 EA-NI-ADRESSAENDGRUND-TAB.
           20 EA-NI-ADRESSAENDGRUND-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-ADRESSAENDGRUND
                 REDEFINES EA-NI-ADRESSAENDGRUND-TAB.
           20 EA-NI-AAGID                    PIC X.
           20 EA-NI-AAGTEXT                  PIC X.
         10 EA-DCL-ADRESSAENDGRUND.
           20 EA-AAGID                       PIC -----9    .
           20 EA-AAGTEXT                     PIC X(30).
      ******************************************************************