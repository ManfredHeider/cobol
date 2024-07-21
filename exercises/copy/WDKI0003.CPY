      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0003.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:16   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AKQUISEDATEN-GESAMT.
         10 EA-NI-AKQUISEDATEN-TAB.
           20 EA-NI-AKQUISEDATEN-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-AKQUISEDATEN
                 REDEFINES EA-NI-AKQUISEDATEN-TAB.
           20 EA-NI-AQUDATENPERSONENID       PIC X.
           20 EA-NI-AQUDATENPERSROLLE        PIC X.
           20 EA-NI-AQUDATENERSTDATUM        PIC X.
           20 EA-NI-AQUKENNZEICHEN           PIC X.
           20 EA-NI-AQULETZTFINSTATDAT       PIC X.
           20 EA-NI-AQULETZTFINSTATERS       PIC X.
           20 EA-NI-AQUZIELLANGFRISTIG       PIC X.
           20 EA-NI-AQUZIELNAECHSTJAHR       PIC X.
           20 EA-NI-AQUKAUFBAUWUNSCH         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AKQUISEDATEN.
           20 EA-AQUDATENPERSONENID          PIC X(13).
           20 EA-AQUDATENPERSROLLE           PIC X(01).
           20 EA-AQUDATENERSTDATUM           PIC X(10).
           20 EA-AQUKENNZEICHEN              PIC X(01).
           20 EA-AQULETZTFINSTATDAT          PIC X(10).
           20 EA-AQULETZTFINSTATERS          PIC X(50).
           20 EA-AQUZIELLANGFRISTIG          PIC X(30).
           20 EA-AQUZIELNAECHSTJAHR          PIC X(30).
           20 EA-AQUKAUFBAUWUNSCH            PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
