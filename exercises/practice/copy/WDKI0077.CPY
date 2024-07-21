      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0077.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0077.CPv  $
      *
      *   Rev 2.3   Apr 07 2003 10:12:38   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.2   Oct 15 2002 08:15:42   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   22 Oct 2001 17:22:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENUEBERTRAGUNG-GESAMT.
         10 EA-NI-KUNDENUEBERTRAGUNG-TAB.
           20 EA-NI-KUNDENUEBERTRAGUNG-ELE PIC X
                  OCCURS 12 TIMES.
         10 EA-NI-KUNDENUEBERTRAGUNG
                 REDEFINES EA-NI-KUNDENUEBERTRAGUNG-TAB.
           20 EA-NI-VERTRAGSID               PIC X.
           20 EA-NI-KDULAUFENDENUMMER        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-VBALTID                  PIC X.
           20 EA-NI-VBALTROLLENKZ            PIC X.
           20 EA-NI-VBNEUID                  PIC X.
           20 EA-NI-VBNEUROLLENKZ            PIC X.
           20 EA-NI-DTAERLEDIGTKZ            PIC X.
           20 EA-NI-KDUEBERTRAGUNGSART       PIC X.
           20 EA-NI-KDUGUELTIGABDAT          PIC X.
           20 EA-NI-KDUBEARBEITER            PIC X.
           20 EA-NI-KDUEIGENVERMKZ           PIC X.
         10 EA-DCL-KUNDENUEBERTRAGUNG.
           20 EA-VERTRAGSID                  PIC X(13).
           20 EA-KDULAUFENDENUMMER           PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-VBALTID                     PIC X(13).
           20 EA-VBALTROLLENKZ               PIC X(1).
           20 EA-VBNEUID                     PIC X(13).
           20 EA-VBNEUROLLENKZ               PIC X(1).
           20 EA-DTAERLEDIGTKZ               PIC X(1).
           20 EA-KDUEBERTRAGUNGSART          PIC X(1).
           20 EA-KDUGUELTIGABDAT             PIC X(010).
           20 EA-KDUBEARBEITER               PIC X(8).
           20 EA-KDUEIGENVERMKZ              PIC X(1).
      *****************************************************************

