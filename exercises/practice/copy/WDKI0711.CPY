      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0711.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0711.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:02   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DOKUMENT-GESAMT.
         10 EA-NI-DOKUMENT-TAB.
           20 EA-NI-DOKUMENT-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-DOKUMENT
                 REDEFINES EA-NI-DOKUMENT-TAB.
           20 EA-NI-DOKUMENTID               PIC X.
           20 EA-NI-ERSTELLDATUM             PIC X.
           20 EA-NI-ERSTELLERROLLE           PIC X.
           20 EA-NI-ERSTELLERID              PIC X.
           20 EA-NI-SUBTYPEDOKUMENT          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DOKUMENT.
           20 EA-DOKUMENTID                  PIC X(13).
           20 EA-ERSTELLDATUM                PIC X(10).
           20 EA-ERSTELLERROLLE              PIC X(01).
           20 EA-ERSTELLERID                 PIC X(13).
           20 EA-SUBTYPEDOKUMENT             PIC X(4).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
