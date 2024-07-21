      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0036.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0036.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:48   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NATPHAUSHALTBEZ-GESAMT.
         10 EA-NI-NATPHAUSHALTBEZ-TAB.
           20 EA-NI-NATPHAUSHALTBEZ-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-NATPHAUSHALTBEZ
                 REDEFINES EA-NI-NATPHAUSHALTBEZ-TAB.
           20 EA-NI-NATPHBPERSONENID         PIC X.
           20 EA-NI-NATPHBHHPERSONENID       PIC X.
           20 EA-NI-NATPHBREIHENFOLGE        PIC X.
           20 EA-NI-NATPHBKINDKZ             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-NATPHAUSHALTBEZ.
           20 EA-NATPHBPERSONENID            PIC X(13).
           20 EA-NATPHBHHPERSONENID          PIC X(13).
           20 EA-NATPHBREIHENFOLGE           PIC -----9    .
           20 EA-NATPHBKINDKZ                PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
