      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0029.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:21:18   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KOMMEINRICHTUNG-GESAMT.
         10 EA-NI-KOMMEINRICHTUNG-TAB.
           20 EA-NI-KOMMEINRICHTUNG-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-KOMMEINRICHTUNG
                 REDEFINES EA-NI-KOMMEINRICHTUNG-TAB.
           20 EA-NI-KOMMEINTELARTID          PIC X.
           20 EA-NI-KOMMEINTELARTTEXT        PIC X.
         10 EA-DCL-KOMMEINRICHTUNG.
           20 EA-KOMMEINTELARTID             PIC -----9    .
           20 EA-KOMMEINTELARTTEXT           PIC X(30).
      ******************************************************************
