      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3573.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3573.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:18:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3573
CCC005*    EXEC SQL DECLARE OVADRESSEN00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,ANREDE               CHAR(1)              NOT NULL
CCC004*     ,ZUNAME               CHAR(40)             NOT NULL
CCC004*     ,VORNAME              CHAR(20)             NOT NULL
CCC004*     ,TITEL_AKAD           CHAR(10)             NOT NULL
CCC004*     ,TITEL_ADEL           CHAR(10)             NOT NULL
CCC004*     ,AUSL_KZ              CHAR(4)              NOT NULL
CCC004*     ,PLZ                  CHAR(9)              NOT NULL
CCC004*     ,ORT                  CHAR(40)             NOT NULL
CCC004*     ,STR                  CHAR(40)             NOT NULL
CCC004*     ,TELEFON_P            CHAR(26)             NOT NULL
CCC004*     ,FAX_NR_P             CHAR(26)             NOT NULL
CCC004*     ,TELEFON_B            CHAR(26)             NOT NULL
CCC004*     ,FAX_NR_B             CHAR(26)             NOT NULL
CCC004*     ,HANDY                CHAR(26)             NOT NULL
CCC004*     ,EMAIL                CHAR(83)             NOT NULL
CCC004*     ,URL                  CHAR(83)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3573
        01 OVADRESSEN00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 ANREDE                            PIC X(1).
         10 ZUNAME                            PIC X(40).
         10 VORNAME                           PIC X(20).
         10 TITEL-AKAD                        PIC X(10).
         10 TITEL-ADEL                        PIC X(10).
         10 AUSL-KZ                           PIC X(4).
         10 PLZ                               PIC X(9).
         10 ORT                               PIC X(40).
         10 STR                               PIC X(40).
         10 TELEFON-P                         PIC X(26).
         10 FAX-NR-P                          PIC X(26).
         10 TELEFON-B                         PIC X(26).
         10 FAX-NR-B                          PIC X(26).
         10 HANDY                             PIC X(26).
         10 EMAIL                             PIC X(83).
         10 URL                               PIC X(83).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
