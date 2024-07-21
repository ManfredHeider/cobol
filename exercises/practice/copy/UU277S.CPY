      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU277S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU277S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:59:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION  02
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : ZV BEWEGUNG                   *
      *  DATEINUMMER       : UU0283                        *
      *  COPYMEMBER        : UU277S                        *
      *  SATZLAENGE        : 90 BYTES                      *
      *  SCHLUESSEL        : UU277S-VBNR                   *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *  ERSTELLDATUM      : 22.05.1991                    *
      *                                                    *
      *  AENDERUNG:                            (VERS    02)*
      *  SATZLAENGE VON 84 AUF 90 BYTE         / MUSALL    *
      *  NEUES FELD: UU277S-ZV-KUEDAT          / 13.01.92  *
      *                                                    *
      ******************************************************


       01  UU277S-REC.
           05  UU277S-DIR              PIC 9(03).
           05  UU277S-STUFE-NR         PIC 9(03).
           05  UU277S-VBNR             PIC 9(07).
           05  UU277S-NACHNAME         PIC X(20).
           05  UU277S-VORNAME          PIC X(20).
           05  UU277S-ZV-KZ            PIC X(02).
           05  UU277S-ZV-BEGINN        PIC X(06).
           05  UU277S-ZV-ABLAUF        PIC X(02).
           05  UU277S-STRUK-STUFE      PIC X(04).
           05  UU277S-PRAXIS-STUFE     PIC X(01).
           05  UU277S-DIR-ALT          PIC X(03).
           05  UU277S-ZV-NEUZUGANG     PIC X(01).
           05  UU277S-ZV-AUSGESCHIEDEN PIC X(01).
           05  UU277S-NAME-NEU         PIC X(01).
           05  UU277S-VORNAME-NEU      PIC X(01).
           05  UU277S-RUECKSTUFUNG     PIC X(01).
           05  UU277S-KUEND-ZV         PIC X(01).
           05  UU277S-KUEND-VERTR      PIC 9(06).
           05  UU277S-ZV-VERAEND       PIC X(01).
           05  UU277S-ZV-KUEDAT        PIC 9(06).

