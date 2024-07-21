      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU261S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU261S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:57:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION  02
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : ZV BESTAND                    *
      *  DATEINUMMER       : UU0261                        *
      *  COPYMEMBER        : UU261S                        *
      *  SATZLAENGE        : 90 BYTES                      *
      *  SCHLUESSEL        : UU261S-VBNR                   *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *  ERSTELLDATUM      : 01.10.1990                    *
      *                                                    *
      *  AENDERUNG:                            (VERS 02)   *
      *  SATZLAENGE VON 80 AUF 90 BYTE       /  MUSALL     *
      *  NEUES FELD: UU261S-ZV-KUEDAT        /  13.01.92   *
      *                                                    *
      ******************************************************


       01  UU261S-REC.
           05  UU261S-DIR              PIC 9(03).
           05  UU261S-STUFE-NR         PIC 9(03).
           05  UU261S-VBNR             PIC 9(07).
           05  UU261S-NACHNAME         PIC X(20).
           05  UU261S-VORNAME          PIC X(20).
           05  UU261S-ZV-KZ            PIC X(02).
           05  UU261S-ZV-BEGINN        PIC 9(06).
           05  UU261S-ZV-ABLAUF        PIC X(02).
           05  UU261S-ZV-STERN         PIC X(03).
           05  UU261S-STRUK-STUFE      PIC X(04).
           05  UU261S-PRAXIS-STUFE     PIC X(01).
           05  UU261S-RUECKSTUFUNG     PIC X(01).
           05  UU261S-KUEND-ZV         PIC X(01).
           05  UU261S-KUEND-VERTR      PIC 9(06).
           05  UU261S-KUEND-BDV        PIC X(01).
           05  UU261S-ZV-KUEDAT        PIC 9(06).
           05  UU261S-FILLER           PIC X(04).

