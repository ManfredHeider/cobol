      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM800S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM800S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:35:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *    UM800S.                                                     *
      *    COPY-MEMBER FUER INTERESSENTEN-/VM-/VB-STAMMDATENSATZ       *
      *    -----------------------------------------------------       *
      *                                                                *
      *    DER STAMMDATENSATZ HAT EINE       ERSTELLT AM 27.06.85      *
      *    LAENGE VON 320 BYTES.             GRUENING (DV-178)         *
      *                                                                *
      *    UMSTELLUNG AUF NEUE PLZ           GEÄNDERT AM 21.06.93      *
      *    NEUE LAENGE 322 BYTES.            SCHMIDT  (VD-178)         *
      *                                                                *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  STAMM-SATZ.
           10  STAMM-KEY.
               20  VBNR                PIC 9(7).
           10  ANREDE                  PIC 99.
           10  NAME                    PIC X(20).
           10  VNAME                   PIC X(15).
           10  NAME1                   PIC X(26).
           10  STR                     PIC X(26).
           10  PLZ                     PIC X(6).
           10  ORT                     PIC X(21).
           10  TELNRP                  PIC X(14).
           10  TELNRG                  PIC X(14).
           10  GEBD                    PIC 9(6).
           10  PNAME                   PIC X(20).
           10  PGEBD                   PIC 9(6).
           10  EDAT                    PIC 9(6).
           10  STRST                   PIC 999.
           10  BEFDAT                  PIC 9(6).
           10  BETR                    PIC 9(7).
           10  BNAME                   PIC X(15).
           10  FELD1                   PIC X(7).
           10  FELD2                   PIC X(7).
           10  FELD3                   PIC X(7).
           10  AENDAT                  PIC 9(6).
           10  PSNR                    PIC 9(6).
           10  FILLER                  PIC X(69).
      *01  FILLER    REDEFINES   STAMM-SATZ.
           02 ST-SATZ         PIC   X(82)        OCCURS 4.

