      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0503.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0503.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:12:56   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-EXTERNERSERVICEBEZ-GESAMT.
         10 EA-NI-EXTERNERSERVICEBEZ-TAB.
           20 EA-NI-EXTERNERSERVICEBEZ-ELE PIC X
                  OCCURS 9 TIMES.
         10 EA-NI-EXTERNERSERVICEBEZ
                 REDEFINES EA-NI-EXTERNERSERVICEBEZ-TAB.
           20 EA-NI-STATISTIKID              PIC X.
           20 EA-NI-EXTERNERSERVICEID        PIC X.
           20 EA-NI-EXTINTSERVICEID          PIC X.
           20 EA-NI-ESBMONAT                 PIC X.
           20 EA-NI-ZAEHLERAUFRUFE           PIC X.
           20 EA-NI-ZAEHLERAUSDRUCKE         PIC X.
           20 EA-NI-ANZAHLAUFRUFE            PIC X.
           20 EA-NI-ANZAHLAUSDRUCKE          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-EXTERNERSERVICEBEZ.
           20 EA-STATISTIKID                 PIC X(13).
           20 EA-EXTERNERSERVICEID           PIC -----9    .
           20 EA-EXTINTSERVICEID             PIC -----9    .
           20 EA-ESBMONAT                    PIC X(10).
           20 EA-ZAEHLERAUFRUFE              PIC --------9 .
           20 EA-ZAEHLERAUSDRUCKE            PIC --------9 .
           20 EA-ANZAHLAUFRUFE               PIC --------9 .
           20 EA-ANZAHLAUSDRUCKE             PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
