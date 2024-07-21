      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0505.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0505.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:00   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-INTERNERSERVICEBEZ-GESAMT.
         10 EA-NI-INTERNERSERVICEBEZ-TAB.
           20 EA-NI-INTERNERSERVICEBEZ-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-INTERNERSERVICEBEZ
                 REDEFINES EA-NI-INTERNERSERVICEBEZ-TAB.
           20 EA-NI-STATISTIKID              PIC X.
           20 EA-NI-INTERNERSERVICEID        PIC X.
           20 EA-NI-ISBMONAT                 PIC X.
           20 EA-NI-ZAEHLERAUFRUFE           PIC X.
           20 EA-NI-ANZAHLAUFRUFE            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-INTERNERSERVICEBEZ.
           20 EA-STATISTIKID                 PIC X(13).
           20 EA-INTERNERSERVICEID           PIC -----9    .
           20 EA-ISBMONAT                    PIC X(10).
           20 EA-ZAEHLERAUFRUFE              PIC --------9 .
           20 EA-ANZAHLAUFRUFE               PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
