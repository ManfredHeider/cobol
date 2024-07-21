      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0072.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0072.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:22:34   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BUNDESLANDPLZ-GESAMT.
         10 EA-NI-BUNDESLANDPLZ-TAB.
           20 EA-NI-BUNDESLANDPLZ-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-BUNDESLANDPLZ
                 REDEFINES EA-NI-BUNDESLANDPLZ-TAB.
           20 EA-NI-LANDID                   PIC X.
           20 EA-NI-PLZVON                   PIC X.
           20 EA-NI-PLZBIS                   PIC X.
           20 EA-NI-LAUFENDENUMMER           PIC X.
           20 EA-NI-BUNDESLANDID             PIC X.
         10 EA-DCL-BUNDESLANDPLZ.
           20 EA-LANDID                      PIC X(3).
           20 EA-PLZVON                      PIC X(15).
           20 EA-PLZBIS                      PIC X(15).
           20 EA-LAUFENDENUMMER              PIC -----9    .
           20 EA-BUNDESLANDID                PIC X(3).
      ******************************************************************
