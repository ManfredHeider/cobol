      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0369.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0369.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:10   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:50   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZAHLUNGSWEISE-GESAMT.
         10 EA-NI-ZAHLUNGSWEISE-TAB.
           20 EA-NI-ZAHLUNGSWEISE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-ZAHLUNGSWEISE
                 REDEFINES EA-NI-ZAHLUNGSWEISE-TAB.
           20 EA-NI-ZAHLUNGSWEISEID          PIC X.
           20 EA-NI-ZAHLUNGSWEISETEXT        PIC X.
           20 EA-NI-ZAHLUNGSWEISEKURZ        PIC X.
         10 EA-DCL-ZAHLUNGSWEISE.
           20 EA-ZAHLUNGSWEISEID             PIC -----9    .
           20 EA-ZAHLUNGSWEISETEXT           PIC X(25).
           20 EA-ZAHLUNGSWEISEKURZ           PIC X(4).
      ******************************************************************
