      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0731.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0731.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:20   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELSTRUKTURAUSWAHL-GESAMT.
         10 EA-NI-SELSTRUKTURAUSWAHL-TAB.
           20 EA-NI-SELSTRUKTURAUSWAHL-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-SELSTRUKTURAUSWAHL
                 REDEFINES EA-NI-SELSTRUKTURAUSWAHL-TAB.
           20 EA-NI-SSAID                    PIC X.
           20 EA-NI-SSABENUTZTANFRAGID       PIC X.
           20 EA-NI-SSAERSTELLPID            PIC X.
           20 EA-NI-SSAERSTELLPROLLE         PIC X.
           20 EA-NI-SSAERSTELLDATUM          PIC X.
           20 EA-NI-SSAFBETREUERPID          PIC X.
           20 EA-NI-SSAFBETREUERPROLLE       PIC X.
           20 EA-NI-SSAFILTSTRUKTARTID       PIC X.
         10 EA-DCL-SELSTRUKTURAUSWAHL.
           20 EA-SSAID                       PIC X(13).
           20 EA-SSABENUTZTANFRAGID          PIC X(13).
           20 EA-SSAERSTELLPID               PIC X(13).
           20 EA-SSAERSTELLPROLLE            PIC X(1).
           20 EA-SSAERSTELLDATUM             PIC X(10).
           20 EA-SSAFBETREUERPID             PIC X(13).
           20 EA-SSAFBETREUERPROLLE          PIC X(1).
           20 EA-SSAFILTSTRUKTARTID          PIC -----9    .
      ******************************************************************
