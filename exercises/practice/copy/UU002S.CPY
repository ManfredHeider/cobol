      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU002S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU002S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:44:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    26.08.94 16:34:12
      *    ----------------------------------------------------------
      *    COPY-MEMBER: BENUTZER-DATEI
      *    ---------------------------
      *                                    ERSTELLT AM 07.12.79
      *                                            VON H.SCHORN / AGS
      *
      *    GEAENDERT AM 26.08.1994                     H.WIECHERS
      *    AUFTRAG: AFRZ0013 - UMSTELLUNG KUS-SYSTEM AUF RACF
      *    DIE BENUTZERDATEI ZUM COPY-MEMBER UU002S EXISTIERT
      *    NICHT MEHR. DAS COPY-MEMBER WIRD ABER NOCH FUER
      *    PROGRAMMINTERNE BEREICHE BENOETIGT.
      *    ----------------------------------------------------------
CCC009*    SKIP1
       01  UU002S.
           10  K2-SA                       PIC 99.
           10  K2-KEY.
260894*        20  K2-PERSNR               PIC 9(7)        COMP-3.
260894*        20  K2-TRANS                PIC 9(6).
260894         20  K2-USERID               PIC X(7).
260894         20  K2-TRANS                PIC 9(6).
260894         20  FILLER                  PIC X(1).
      *
      *    BESTANDS-SATZ
      *
           10  K2-SATZ.
260894*        20  K2-CODEWORT             PIC X(4).
               20  K2-ABTDIR               PIC X(5).
               20  K2-VARPRUEF.
                   30  K2-VP-DIR1          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR2          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR3          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR4          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR5          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR6          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR7          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR8          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR9          PIC 9(3)       COMP-3.
                   30  K2-VP-DIR10         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR11         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR12         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR13         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR14         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR15         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR16         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR17         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR18         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR19         PIC 9(3)       COMP-3.
                   30  K2-VP-DIR20         PIC 9(3)       COMP-3.
                   30  K2-VP-VBNR          PIC 9(7)       COMP-3.
                   30  K2-VP-RES1          PIC X(26).
               20  K2-STATUS               PIC X.
               20  K2-RESERVE              PIC X(28).
      *
      *    DUMMY-SATZ
      *
           10  K2-DUMMY  REDEFINES  K2-SATZ.
               20  K2-ANZAHL               PIC 9(7)        COMP-3.
               20  K2-LADEDAT              PIC 9(7)        COMP-3.
               20  K2-ELADEDAT             PIC 9(7)        COMP-3.

