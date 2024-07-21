      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0501.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0501.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:12:54   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DFUEVERBINDUNG-GESAMT.
         10 EA-NI-DFUEVERBINDUNG-TAB.
           20 EA-NI-DFUEVERBINDUNG-ELE PIC X
                  OCCURS 15 TIMES.
         10 EA-NI-DFUEVERBINDUNG
                 REDEFINES EA-NI-DFUEVERBINDUNG-TAB.
           20 EA-NI-VBID                     PIC X.
           20 EA-NI-VBROLLE                  PIC X.
           20 EA-NI-DATUMAUFBAU              PIC X.
           20 EA-NI-ZEITAUFBAU               PIC X.
           20 EA-NI-ZEITABBAU                PIC X.
           20 EA-NI-WAEHRUNGSKZ              PIC X.
           20 EA-NI-GEBUEHREN                PIC X.
           20 EA-NI-VBNUMMER                 PIC X.
           20 EA-NI-VBNAME                   PIC X.
           20 EA-NI-VBVORNAME                PIC X.
           20 EA-NI-PCBESITZERID             PIC X.
           20 EA-NI-PCBESITZERROLLE          PIC X.
           20 EA-NI-BEZEICHNUNG              PIC X.
           20 EA-NI-WORKSTATIONID            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DFUEVERBINDUNG.
           20 EA-VBID                        PIC X(13).
           20 EA-VBROLLE                     PIC X(01).
           20 EA-DATUMAUFBAU                 PIC X(10).
           20 EA-ZEITAUFBAU                  PIC X(08).
           20 EA-ZEITABBAU                   PIC X(08).
           20 EA-WAEHRUNGSKZ                 PIC X(1).
           20 EA-GEBUEHREN                   PIC ---------9.99    .
           20 EA-VBNUMMER                    PIC --------9 .
           20 EA-VBNAME                      PIC X(30).
           20 EA-VBVORNAME                   PIC X(15).
           20 EA-PCBESITZERID                PIC X(13).
           20 EA-PCBESITZERROLLE             PIC X(01).
           20 EA-BEZEICHNUNG.
              49 EA-BEZEICHNUNG-LEN          PIC -----9    .
              49 EA-BEZEICHNUNG-TEXT         PIC X(50).
           20 EA-WORKSTATIONID               PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
