      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3599.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTMA3599.CPv  $
      *
      *   Rev 2.3   Oct 20 2017 08:01:16   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - DCL für Tabellen
      *
      *   Rev 2.2   Jul 28 2015 16:03:12   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 11:20:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00129!07.07.15!2.2  ! neue Attribute:           ! D.WILHELM
      *         !        !     ! DISKONT_BETR_BRUTTO       !
      *         !        !     ! DISKONT_JBETR_BRUTTO      !
      *         !        !     ! DISKONT_BETR_NETTO        !
      *         !        !     ! DISKONT_JBETR_NETTO       !
      *         !        !     ! DISKONT_BETR_UST          !
      *         !        !     ! DISKONT_JBETR_UST         !
      *         !        !     ! gelöscht:                 !
      *         !        !     ! DISKONT_BETR              !
      *         !        !     ! DISKONT_JBETR             !
      * AMA00360!10.10.17!2.3  ! Umbenennung RUECK_BETR    ! D.WILHELM
      *         !        !     !             RUECK_JBETR   !
      *         !        !     ! in          EINB_BETR     !
      *         !        !     !             EINB_JBETR    !
      *         !        !     !                           !
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OVBUCHDATWERTE00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,IND_JW               SMALLINT             NOT NULL
CCC004*     ,BUCH_DAT             DATE                 NOT NULL
CCC004*     ,DISKONT_BETR_BRUTTO  DECIMAL(13, 2)       NOT NULL
CCC004*     ,DISKONT_JBETR_BRUTTO DECIMAL(13, 2)       NOT NULL
CCC004*     ,DISKONT_BETR_NETTO   DECIMAL(13, 2)       NOT NULL
CCC004*     ,DISKONT_JBETR_NETTO  DECIMAL(13, 2)       NOT NULL
CCC004*     ,DISKONT_BETR_UST     DECIMAL(13, 2)       NOT NULL
CCC004*     ,DISKONT_JBETR_UST    DECIMAL(13, 2)       NOT NULL
CCC004*     ,EINB_BETR            DECIMAL(13, 2)       NOT NULL
CCC004*     ,EINB_JBETR           DECIMAL(13, 2)       NOT NULL
CCC004*     ,DARLEHEN_BETR        DECIMAL(13, 2)       NOT NULL
CCC004*     ,DARLEHEN_JBETR       DECIMAL(13, 2)       NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      *
        01 OVBUCHDATWERTE00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 IND-JW                            PIC S9(4) COMP.
         10 BUCH-DAT                          PIC X(10).
         10 DISKONT-BETR-BRUTTO               PIC S9(11)V9(2) COMP-3.
         10 DISKONT-JBETR-BRUTTO              PIC S9(11)V9(2) COMP-3.
         10 DISKONT-BETR-NETTO                PIC S9(11)V9(2) COMP-3.
         10 DISKONT-JBETR-NETTO               PIC S9(11)V9(2) COMP-3.
         10 DISKONT-BETR-UST                  PIC S9(11)V9(2) COMP-3.
         10 DISKONT-JBETR-UST                 PIC S9(11)V9(2) COMP-3.
         10 EINB-BETR                         PIC S9(11)V9(2) COMP-3.
         10 EINB-JBETR                        PIC S9(11)V9(2) COMP-3.
         10 DARLEHEN-BETR                     PIC S9(11)V9(2) COMP-3.
         10 DARLEHEN-JBETR                    PIC S9(11)V9(2) COMP-3.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
 

