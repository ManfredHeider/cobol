      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0385.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0385.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:56:52   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 16 1999 10:41:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATSEITE-GESAMT.
         10 EA-NI-FINDATSEITE-TAB.
           20 EA-NI-FINDATSEITE-ELE PIC X
                  OCCURS 11 TIMES.
         10 EA-NI-FINDATSEITE
                 REDEFINES EA-NI-FINDATSEITE-TAB.
           20 EA-NI-FDSSEITEID               PIC X.
           20 EA-NI-FDSTABELLE               PIC X.
           20 EA-NI-FDSLFDNUMMER             PIC X.
           20 EA-NI-FDSZUSATZDATUM           PIC X.
           20 EA-NI-FDSGRUPPEID              PIC X.
           20 EA-NI-FDSBESCHREIBUNG          PIC X.
           20 EA-NI-FDSFORM                  PIC X.
           20 EA-NI-FDSTEXTBESCHRIFT         PIC X.
           20 EA-NI-FDSSTAND                 PIC X.
           20 EA-NI-FDSTEXT                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATSEITE.
           20 EA-FDSSEITEID                  PIC X(13).
           20 EA-FDSTABELLE                  PIC X(13).
           20 EA-FDSLFDNUMMER                PIC -----9    .
           20 EA-FDSZUSATZDATUM              PIC X(10).
           20 EA-FDSGRUPPEID                 PIC X(13).
           20 EA-FDSBESCHREIBUNG.
              49 EA-FDSBESCHREIBUNG-LEN      PIC -----9    .
              49 EA-FDSBESCHREIBUNG-TEXT     PIC X(80).
           20 EA-FDSFORM.
              49 EA-FDSFORM-LEN              PIC -----9    .
              49 EA-FDSFORM-TEXT             PIC X(40).
           20 EA-FDSTEXTBESCHRIFT.
              49 EA-FDSTEXTBESCHRIFT-LEN     PIC -----9    .
              49 EA-FDSTEXTBESCHRIFT-TEXT    PIC X(40).
           20 EA-FDSSTAND                    PIC X(10).
           20 EA-FDSTEXT.
              49 EA-FDSTEXT-LEN              PIC -----9    .
              49 EA-FDSTEXT-TEXT             PIC X(3500).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
