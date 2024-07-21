      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0702.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0702.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:52   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DOKUMENTPERSON-GESAMT.
         10 EA-NI-DOKUMENTPERSON-TAB.
           20 EA-NI-DOKUMENTPERSON-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-DOKUMENTPERSON
                 REDEFINES EA-NI-DOKUMENTPERSON-TAB.
           20 EA-NI-DOKUMENTID               PIC X.
           20 EA-NI-PERSONENID               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DOKUMENTPERSON.
           20 EA-DOKUMENTID                  PIC X(13).
           20 EA-PERSONENID                  PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
