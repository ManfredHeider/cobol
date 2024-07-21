      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3585.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3585.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 16:02:40   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 11:19:20   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00129!29.06.15!2.2  ! Änderung wg. Lucy         ! D.WILHELM
      *         !        !     ! ABR-NR-KTO-H              !
      *         !        !     ! ABR-NR-KTO-D              !
      *         !        !     ! ABR-NR-KTO-A              !
      *         !        !     ! von S9(4) bin auf X(15)   !
      *         !        !     !                           !
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OVKONTEN00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,ABR_NR_KTO_H         CHAR(15)             NOT NULL
CCC004*     ,ABR_NR_KTO_D         CHAR(15)             NOT NULL
CCC004*     ,ABR_NR_KTO_A         CHAR(15)             NOT NULL
CCC004*     ,LAST_BUCH_DAT_ABR    DATE                 NOT NULL
CCC004*     ,LAST_BUCH_DAT_PROV   DATE                 NOT NULL
CCC004*     ,NUR_SALDOBUCH_KZN    CHAR(1)              NOT NULL
CCC004*     ,KTO_STAND_H          DECIMAL(11, 2)       NOT NULL
CCC004*     ,BUCH_DAT_H           DATE                 NOT NULL
CCC004*     ,KTO_STAND_R          DECIMAL(11, 2)       NOT NULL
CCC004*     ,BUCH_DAT_R           DATE                 NOT NULL
CCC004*     ,KTO_STAND_D          DECIMAL(11, 2)       NOT NULL
CCC004*     ,BUCH_DAT_D           DATE                 NOT NULL
CCC004*     ,KTO_STAND_A          DECIMAL(11, 2)       NOT NULL
CCC004*     ,BUCH_DAT_A           DATE                 NOT NULL
CCC004*     ,UEB_BETRAG           DECIMAL(11, 2)       NOT NULL
CCC004*     ,UEB_DATUM            DATE                 NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      *
        01 OVKONTEN00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 ABR-NR-KTO-H                      PIC X(15).
         10 ABR-NR-KTO-D                      PIC X(15).
         10 ABR-NR-KTO-A                      PIC X(15).
         10 LAST-BUCH-DAT-ABR                 PIC X(10).
         10 LAST-BUCH-DAT-PROV                PIC X(10).
         10 NUR-SALDOBUCH-KZN                 PIC X(1).
         10 KTO-STAND-H                       PIC S9(9)V9(2) COMP-3.
         10 BUCH-DAT-H                        PIC X(10).
         10 KTO-STAND-R                       PIC S9(9)V9(2) COMP-3.
         10 BUCH-DAT-R                        PIC X(10).
         10 KTO-STAND-D                       PIC S9(9)V9(2) COMP-3.
         10 BUCH-DAT-D                        PIC X(10).
         10 KTO-STAND-A                       PIC S9(9)V9(2) COMP-3.
         10 BUCH-DAT-A                        PIC X(10).
         10 UEB-BETRAG                        PIC S9(9)V9(2) COMP-3.
         10 UEB-DATUM                         PIC X(10).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

