      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3581.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3581.CPv  $
      *
      *   Rev 2.2   Oct 20 2017 08:01:04   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - DCL für Tabellen
      *
      *   Rev 2.1   Jun 28 2013 11:19:08   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00360!10.10.17!2.2  ! Umbenennen                ! D.WILHELM
      *         !        !     ! OVRUECKSTELLUNG00         !
      *         !        !     ! in OVEINBEHALT00          !
      *         !        !     ! Umbenennen RUECK-SATZ     !
      *         !        !     ! in EINB-SATZ              !
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3581
CCC005*    EXEC SQL DECLARE OVEINBEHALT00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,EINB_SATZ            DECIMAL(5, 2)        NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3581
        01 OVEINBEHALT00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 EINB-SATZ                         PIC S9(3)V9(2) COMP-3.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

