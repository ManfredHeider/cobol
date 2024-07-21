      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0701.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0701.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:50   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ARBAUFTRAGEMPFBEZ-GESAMT.
         10 EA-NI-ARBAUFTRAGEMPFBEZ-TAB.
           20 EA-NI-ARBAUFTRAGEMPFBEZ-ELE PIC X
                  OCCURS 9 TIMES.
         10 EA-NI-ARBAUFTRAGEMPFBEZ
                 REDEFINES EA-NI-ARBAUFTRAGEMPFBEZ-TAB.
           20 EA-NI-ARBEMPFBEZID             PIC X.
           20 EA-NI-ARBEITSAUFTRAGID         PIC X.
           20 EA-NI-EMPFAENGERID             PIC X.
           20 EA-NI-EMPFAENGERROLLE          PIC X.
           20 EA-NI-RUECKMELDUNG             PIC X.
           20 EA-NI-STATUSID                 PIC X.
           20 EA-NI-ERLEDIGTAMDATUM          PIC X.
           20 EA-NI-STATUSAENDERNDATUM       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-ARBAUFTRAGEMPFBEZ.
           20 EA-ARBEMPFBEZID                PIC X(13).
           20 EA-ARBEITSAUFTRAGID            PIC X(13).
           20 EA-EMPFAENGERID                PIC X(13).
           20 EA-EMPFAENGERROLLE             PIC X(01).
           20 EA-RUECKMELDUNG                PIC X(01).
           20 EA-STATUSID                    PIC --------9 .
           20 EA-ERLEDIGTAMDATUM             PIC X(10).
           20 EA-STATUSAENDERNDATUM          PIC X(10).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
