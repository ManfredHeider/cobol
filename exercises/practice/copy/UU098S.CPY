      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU098S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU098S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:54:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    26.08.94 16:36:11
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BANKDIENSTLEISTUNGEN - TRANSAKTION UU81    *
      *------------------------------------------------------------    *
      *    DATEINAME   U.UU0014 -KSDS                                  *
      *    SATZART     99                                              *
      *    LAENGE      1409 BYTES      ERSTELLT AM 12.09.89 - GRUENING *
      *                                                                *
      *    GEAENDERT   26.08.94 / WIECHERS / AUFTRAG AFRZ0013          *
      *                KUS-SYSTEM / RACF                               *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU098S.
           05 INFOSATZ.
              10  SA                  PIC 99.
              10  INFO-KEY.
                  20 BILD             PIC 99.
                  20 LFDNR            PIC 999.
              10  FOLGENR             PIC 999.
260894*       10  PERSNR              PIC 9(7).
260894        10  USERID              PIC X(7).
              10  DATUM               PIC 9(6).
              10  ZEILEN          OCCURS 18.
                  20 ZEIL             PIC X(77).
           05  DUMSATZ    REDEFINES INFOSATZ.
               10 FILLER              PIC 9(17).
               10 DUMDAT              PIC 9(6).

