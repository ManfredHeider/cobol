      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU295S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\UU295S.CPv  $
      *
      *   Rev 2.1   21 Dec 2000 11:06:20   T13AE27
      *AVBG-Anpassung
      *
      *   Rev 2.0   Feb 27 1999 15:15:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:59:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : DIREKTE-LINIEN-DATEI          *
      *  DATEINUMMER       : UU0295                        *
      *  COPYMEMBER        : UU295S                        *
      *  SATZLAENGE        : 160 BYTES                     *
      *  SCHLUESSEL        : UU295S-KEY                    *
      *  SCHLUESSELLAENGE  :   9 BYTES                     *
      *  ERSTELLDATUM      : 26.06.1991                    *
      *                                                    *
      ******************************************************

       01  UU295S.
           05  DIR-L-SATZ.
               10  UU295S-KEY.
                   15  UU295S-VBNR         PIC 9(07).
                   15  UU295S-ZAEHLER      PIC 99.
               10  UU295S-DIR-LINIE OCCURS 5.
011100             15 UU295S-U-GES         PIC 9(004) COMP.
011100             15 UU295S-U-WAEHR       PIC 9(004) COMP.
                   15 UU295S-U-VBNR        PIC 9(07).
                   15 UU295S-U-B-ZUSCHUSS  PIC 9(07)V9.
                   15 UU295S-U-ABZUG       PIC 9(07)V9.
011100*        10  FILLER                  PIC X(36).
011100         10  UU295S-GES              PIC 9(004) COMP.
011100         10  UU295S-WAEHR            PIC 9(004) COMP.
               10  FILLER                  PIC X(12).
