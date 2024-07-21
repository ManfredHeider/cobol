      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3603.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3603.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:20:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3603
CCC005*    EXEC SQL DECLARE OVEVWS_INFO00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,FOLGE                CHAR(1)              NOT NULL
CCC004*     ,VERTRAGSNR           CHAR(10)             NOT NULL
CCC004*     ,INKST                DATE                 NOT NULL
CCC004*     ,ZUST                 CHAR(4)              NOT NULL
CCC004*     ,VN_NAME              CHAR(40)             NOT NULL
CCC004*     ,VN_VORNAME           CHAR(25)             NOT NULL
CCC004*     ,VN_STRASSE           CHAR(40)             NOT NULL
CCC004*     ,VN_PLZ               CHAR(5)              NOT NULL
CCC004*     ,VN_ORT               CHAR(34)             NOT NULL
CCC004*     ,VT_1_NAME            CHAR(40)             NOT NULL
CCC004*     ,VT_1_VORNAME         CHAR(25)             NOT NULL
CCC004*     ,VT_1_STRASSE         CHAR(40)             NOT NULL
CCC004*     ,VT_1_PLZ             CHAR(5)              NOT NULL
CCC004*     ,VT_1_ORT             CHAR(34)             NOT NULL
CCC004*     ,VT_2_NAME            CHAR(40)             NOT NULL
CCC004*     ,VT_2_VORNAME         CHAR(25)             NOT NULL
CCC004*     ,VT_2_STRASSE         CHAR(40)             NOT NULL
CCC004*     ,VT_2_PLZ             CHAR(5)              NOT NULL
CCC004*     ,VT_2_ORT             CHAR(34)             NOT NULL
CCC004*     ,VT_3_NAME            CHAR(40)             NOT NULL
CCC004*     ,VT_3_VORNAME         CHAR(25)             NOT NULL
CCC004*     ,VT_3_STRASSE         CHAR(40)             NOT NULL
CCC004*     ,VT_3_PLZ             CHAR(5)              NOT NULL
CCC004*     ,VT_3_ORT             CHAR(34)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3603
        01 OVEVWS-INFO00-SATZ.
         10 VERSION                           PIC X(1).
         10 FOLGE                             PIC X(1).
         10 VERTRAGSNR                        PIC X(10).
         10 INKST                             PIC X(10).
         10 ZUST                              PIC X(4).
         10 VN-NAME                           PIC X(40).
         10 VN-VORNAME                        PIC X(25).
         10 VN-STRASSE                        PIC X(40).
         10 VN-PLZ                            PIC X(5).
         10 VN-ORT                            PIC X(34).
         10 VT-1-NAME                         PIC X(40).
         10 VT-1-VORNAME                      PIC X(25).
         10 VT-1-STRASSE                      PIC X(40).
         10 VT-1-PLZ                          PIC X(5).
         10 VT-1-ORT                          PIC X(34).
         10 VT-2-NAME                         PIC X(40).
         10 VT-2-VORNAME                      PIC X(25).
         10 VT-2-STRASSE                      PIC X(40).
         10 VT-2-PLZ                          PIC X(5).
         10 VT-2-ORT                          PIC X(34).
         10 VT-3-NAME                         PIC X(40).
         10 VT-3-VORNAME                      PIC X(25).
         10 VT-3-STRASSE                      PIC X(40).
         10 VT-3-PLZ                          PIC X(5).
         10 VT-3-ORT                          PIC X(34).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
