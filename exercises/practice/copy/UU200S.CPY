      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU200S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU200S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:56:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER PUNCH-SAETZE (WETTBEWERB 87)               *
      *    ---------------------------------------------               *
      *                                                                *
      *    LAENGE = 72 BYTES       - ERSTELLT AM 26.01.87 / BIEHL      *
      *                                                                *
      *                            - VON UM152S IN UM802S UMBENANNT    *
      *                              AM 16.02.87 / BIEHL               *
      *                                                                *
      *                            - SATZAUFBAU GEAENDERT (NEUE FELDER)*
      *                              - PCH-ESACH, PCH-ESACH-VORZ       *
      *                                PCH-ECKV, PCH-ECKV-VORZ         *
      *                                29.01.88 / GRUENING - WETTB.88  *
      *                                                                *
      *                            - VON UM802S IN UU200S UMBENANNT    *
      *                                AM 29.01.88 / GRUENING          *
      *                            - NEU KATALOGISIERT                 *
      *                                AM 29.01.88 / GRUENING          *
      *                                                                *
      *                            - SATZAUFBAU GEAENDERT (NEUE FELDER)*
      *                              - PCH-STOEIGEN-M                  *
      *                                PCH-STOEIGEN-M-VORZ             *
      *                                AM 10.02.88 / HECKER - WETTB.89 *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU200S.
           10  PCH-SA                  PIC 99.
           10  PCH-VBNR                PIC 9(7).
           10  PCH-DAT                 PIC 9(4).
           10  PCH-SATZ1.
               20  PCH-VBNAM               PIC X(20).
               20  PCH-VBDIR               PIC 999.
               20  PCH-VBSTUF              PIC 999.
               20  PCH-BETR                PIC 9(7).
               20  PCH-BETRNAM             PIC X(20).
               20  PCH-VBSTUFDAT           PIC 9(6).
           10  PCH-SATZ2 REDEFINES PCH-SATZ1.
               20  PCH-EEIGEN              PIC 9(7)V99.
               20  PCH-EEIGEN-VORZ         PIC 9.
               20  PCH-STOEIGEN            PIC 9(3)V99.
               20  PCH-STOEIGEN-VORZ       PIC 9.
               20  PCH-EGRUPPE             PIC 9(7)V99.
               20  PCH-EGRUPPE-VORZ        PIC 9.
               20  PCH-STOGRUPPE           PIC 9(3)V99.
               20  PCH-STOGRUPPE-VORZ      PIC 9.
               20  PCH-ESACH               PIC 9(7)V99.
               20  PCH-ESACH-VORZ          PIC 9.
               20  PCH-ECKV                PIC 9(7)V99.
               20  PCH-ECKV-VORZ           PIC 9.
               20  PCH-STOEIGEN-M          PIC 9(3)V99.
               20  PCH-STOEIGEN-M-VORZ     PIC 9.
               20  FILLER                  PIC X.

