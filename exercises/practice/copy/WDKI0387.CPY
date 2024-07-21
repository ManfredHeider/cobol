      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0387.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0387.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:56:56   I13PC19
      * 
      *
      *   Rev 2.0   Apr 16 1999 10:41:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATTABELLE-GESAMT.
         10 EA-NI-FINDATTABELLE-TAB.
           20 EA-NI-FINDATTABELLE-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-FINDATTABELLE
                 REDEFINES EA-NI-FINDATTABELLE-TAB.
           20 EA-NI-FDTATABELLEID            PIC X.
           20 EA-NI-FDTABREITE               PIC X.
           20 EA-NI-FDTABEZEICHNUNG          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATTABELLE.
           20 EA-FDTATABELLEID               PIC X(13).
           20 EA-FDTABREITE                  PIC -----9    .
           20 EA-FDTABEZEICHNUNG.
              49 EA-FDTABEZEICHNUNG-LEN      PIC -----9    .
              49 EA-FDTABEZEICHNUNG-TEXT     PIC X(100).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
