      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0074.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0074.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:52   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NATPNEBENBERUF-GESAMT.
         10 EA-NI-NATPNEBENBERUF-TAB.
           20 EA-NI-NATPNEBENBERUF-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-NATPNEBENBERUF
                 REDEFINES EA-NI-NATPNEBENBERUF-TAB.
           20 EA-NI-NATPNBPERSONENID         PIC X.
           20 EA-NI-NATPNEBENBERUFTEXT       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-NATPNEBENBERUF.
           20 EA-NATPNBPERSONENID            PIC X(13).
           20 EA-NATPNEBENBERUFTEXT          PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
