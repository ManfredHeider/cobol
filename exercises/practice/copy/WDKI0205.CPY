      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0205.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0205.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:10   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDEVBMAGAZIN-GESAMT.
         10 EA-NI-KUNDEVBMAGAZIN-TAB.
           20 EA-NI-KUNDEVBMAGAZIN-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-KUNDEVBMAGAZIN
                 REDEFINES EA-NI-KUNDEVBMAGAZIN-TAB.
           20 EA-NI-KUNDEINTID               PIC X.
           20 EA-NI-KUNDEINTROLLE            PIC X.
           20 EA-NI-MITARBID                 PIC X.
           20 EA-NI-MITARBROLLE              PIC X.
           20 EA-NI-MAGAZIN                  PIC X.
           20 EA-NI-ANZAHL                   PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KUNDEVBMAGAZIN.
           20 EA-KUNDEINTID                  PIC X(13).
           20 EA-KUNDEINTROLLE               PIC X(1).
           20 EA-MITARBID                    PIC X(13).
           20 EA-MITARBROLLE                 PIC X(1).
           20 EA-MAGAZIN                     PIC X(1).
           20 EA-ANZAHL                      PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
