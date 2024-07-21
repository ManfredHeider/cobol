      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0507.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0507.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:06   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LERNDATEN-GESAMT.
         10 EA-NI-LERNDATEN-TAB.
           20 EA-NI-LERNDATEN-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-LERNDATEN
                 REDEFINES EA-NI-LERNDATEN-TAB.
           20 EA-NI-STATISTIKID              PIC X.
           20 EA-NI-LDMONAT                  PIC X.
           20 EA-NI-KAPITEL                  PIC X.
           20 EA-NI-ANZMINUTENGESAMT         PIC X.
           20 EA-NI-ANZMINUTEN               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-LERNDATEN.
           20 EA-STATISTIKID                 PIC X(13).
           20 EA-LDMONAT                     PIC X(10).
           20 EA-KAPITEL                     PIC X(10).
           20 EA-ANZMINUTENGESAMT            PIC --------9 .
           20 EA-ANZMINUTEN                  PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
