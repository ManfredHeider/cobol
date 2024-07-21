      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0714.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0714.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:04   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-RUECKLAUFKONTROLLE-GESAMT.
         10 EA-NI-RUECKLAUFKONTROLLE-TAB.
           20 EA-NI-RUECKLAUFKONTROLLE-ELE PIC X
                  OCCURS 15 TIMES.
         10 EA-NI-RUECKLAUFKONTROLLE
                 REDEFINES EA-NI-RUECKLAUFKONTROLLE-TAB.
           20 EA-NI-RUECKLAUFKONTRID         PIC X.
           20 EA-NI-GESELLSCHAFT             PIC X.
           20 EA-NI-VERSICHERUNGSNR          PIC X.
           20 EA-NI-ABSENDEDATUM             PIC X.
           20 EA-NI-GESELLSCHAFTSKZ          PIC X.
           20 EA-NI-KUNDENNUMMER             PIC X.
           20 EA-NI-VBNUMMER                 PIC X.
           20 EA-NI-BESCHREIBUNG             PIC X.
           20 EA-NI-ERLEDIGUNGSKZ            PIC X.
           20 EA-NI-ERLEDIGTAMDATUM          PIC X.
           20 EA-NI-KUNDENNAME               PIC X.
           20 EA-NI-BESUCHSAUFTRART          PIC X.
           20 EA-NI-PCVWPC                   PIC X.
           20 EA-NI-PCVBPC                   PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-RUECKLAUFKONTROLLE.
           20 EA-RUECKLAUFKONTRID            PIC X(13).
           20 EA-GESELLSCHAFT                PIC -----9    .
           20 EA-VERSICHERUNGSNR             PIC X(15).
           20 EA-ABSENDEDATUM                PIC X(10).
           20 EA-GESELLSCHAFTSKZ             PIC -----9    .
           20 EA-KUNDENNUMMER                PIC --------9 .
           20 EA-VBNUMMER                    PIC --------9 .
           20 EA-BESCHREIBUNG                PIC X(01).
           20 EA-ERLEDIGUNGSKZ               PIC -----9    .
           20 EA-ERLEDIGTAMDATUM             PIC X(10).
           20 EA-KUNDENNAME                  PIC X(30).
           20 EA-BESUCHSAUFTRART             PIC X(01).
           20 EA-PCVWPC                      PIC --------9 .
           20 EA-PCVBPC                      PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
