      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0009.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:18   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BANKVERBINDUNG-GESAMT.
         10 EA-NI-BANKVERBINDUNG-TAB.
           20 EA-NI-BANKVERBINDUNG-ELE PIC X
                  OCCURS 9 TIMES.
         10 EA-NI-BANKVERBINDUNG
                 REDEFINES EA-NI-BANKVERBINDUNG-TAB.
           20 EA-NI-BVPERSONENID             PIC X.
           20 EA-NI-BVLAUFENDENUMMER         PIC X.
           20 EA-NI-BVKONTONUMMER            PIC X.
           20 EA-NI-BVBANKBLZID              PIC X.
           20 EA-NI-BVKONTOARTID             PIC X.
           20 EA-NI-BVINSTITUTBLZ            PIC X.
           20 EA-NI-BVINSTITUTNAME           PIC X.
           20 EA-NI-BVPARTGESELLSCHID        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-BANKVERBINDUNG.
           20 EA-BVPERSONENID                PIC X(13).
           20 EA-BVLAUFENDENUMMER            PIC --------9 .
           20 EA-BVKONTONUMMER               PIC X(15).
           20 EA-BVBANKBLZID                 PIC --------9 .
           20 EA-BVKONTOARTID                PIC -----9    .
           20 EA-BVINSTITUTBLZ               PIC --------9 .
           20 EA-BVINSTITUTNAME              PIC X(50).
           20 EA-BVPARTGESELLSCHID           PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
