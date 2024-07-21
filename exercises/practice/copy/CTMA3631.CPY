      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3631.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3631.CPv  $
      *
      *   Rev 2.2   Jul 07 2016 16:05:48   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.1   Jun 28 2013 11:22:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00292!10.05.16!2.2  ! Umbenennung von           ! D.WILHELM
      *         !        !     ! OVGESELLSCHAFT00 in       !
      *         !        !     ! OVPRODUKTGEBER00          !
      *         !        !     ! neue Attribute: VERSION,  !
      *         !        !     ! VG, etc.                  !
      *         !        !     !                           !
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3631
CCC005*    EXEC SQL DECLARE OVPRODUKTGEBER00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,PG                   SMALLINT             NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,PG_KURZ              CHAR(10)             NOT NULL
CCC004*     ,PG_LANG              CHAR(40)             NOT NULL
CCC004*     ,LAND_KZN             CHAR(4)              NOT NULL
CCC004*     ,BEW_ART_KZN          CHAR(10)             NOT NULL
CCC004*     ,KTO                  CHAR(10)             NOT NULL
CCC004*     ,PG_UST_RELEVANT      CHAR(1)              NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3631
        01 OVPRODUKTGEBER00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 PG                                PIC S9(4) COMP.
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 PG-KURZ                           PIC X(10).
         10 PG-LANG                           PIC X(40).
         10 LAND-KZN                          PIC X(4).
         10 BEW-ART-KZN                       PIC X(10).
         10 KTO                               PIC X(10).
         10 PG-UST-RELEVANT                   PIC X(1).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

