      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU201S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU201S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:56:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *    UU201S.                                                     *
      *    COPY-MEMBER FUER STRUKTURBESTANDSSATZ                       *
      *    -------------------------------------                       *
      *                                                                *
      *    DER STRUTURBESTANDSSATZ HAT       ERSTELLT AM 27.06.85      *
      *    EINE LAENGE VON 80 BYTES.         GRUENING (DV-178)         *
      *    GEAENDERT AUF 25 BYTES            24.02.87 - GRUENING       *
      *    COPY-MEMBERNAME IN UU201S                                   *
      *    GEAENDERT                         25.05.88 - GRUENING       *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  STRUK-SATZ.
           10  S-KEYST.
               20  S-BETR              PIC 9(7).
               20  S-STRST             PIC 999.
               20  S-VBNR              PIC 9(7).
           10  S-FREI                  PIC 9(8).

