      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0725.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0725.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:31:22   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TMPSELZUSATZ-GESAMT.
         10 EA-NI-TMPSELZUSATZ-TAB.
           20 EA-NI-TMPSELZUSATZ-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-TMPSELZUSATZ
                 REDEFINES EA-NI-TMPSELZUSATZ-TAB.
           20 EA-NI-ZUSATZPERSONENID         PIC X.
           20 EA-NI-ZUSATZKUNDENNUMMER       PIC X.
           20 EA-NI-ZUSATZBETREUERNR         PIC X.
           20 EA-NI-ZUSATZANZEIGENAME        PIC X.
         10 EA-DCL-TMPSELZUSATZ.
           20 EA-ZUSATZPERSONENID            PIC X(13).
           20 EA-ZUSATZKUNDENNUMMER          PIC --------9 .
           20 EA-ZUSATZBETREUERNR            PIC --------9 .
           20 EA-ZUSATZANZEIGENAME           PIC X(40).
      ******************************************************************
