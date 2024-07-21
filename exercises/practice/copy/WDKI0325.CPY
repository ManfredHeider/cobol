      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0325.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0325.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:25:00   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KLAUSEL-GESAMT.
         10 EA-NI-KLAUSEL-TAB.
           20 EA-NI-KLAUSEL-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-KLAUSEL
                 REDEFINES EA-NI-KLAUSEL-TAB.
           20 EA-NI-KLAPARTNERGESID          PIC X.
           20 EA-NI-KLAKLAUSELID             PIC X.
           20 EA-NI-KLAKLAUSELTEXT           PIC X.
         10 EA-DCL-KLAUSEL.
           20 EA-KLAPARTNERGESID             PIC -----9    .
           20 EA-KLAKLAUSELID                PIC --------9 .
           20 EA-KLAKLAUSELTEXT              PIC X(30).
      ******************************************************************
