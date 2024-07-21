      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3637.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3637.CPv  $
      *
      *   Rev 2.2   Oct 20 2017 08:01:38   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - DCL für Tabellen
      *
      *   Rev 2.1   Jun 28 2013 11:22:34   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00360!10.10.17!2.2  ! Umbenennen                ! D.WILHELM
      *         !        !     ! OVPROD_RUECK_REF00        !
      *         !        !     ! in OVPROD_EINB_REF00      !
      *         !        !     !                           !
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3637
CCC005*    EXEC SQL DECLARE OVPROD_EINB_REF00 TABLE
CCC004*     (PG                   SMALLINT             NOT NULL
CCC004*     ,PRODSCHL             SMALLINT             NOT NULL
CCC004*     ,SEL_WERT             CHAR(15)             NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,PROD_KURZ            CHAR(10)             NOT NULL
CCC004*     ,PROD_LANG            CHAR(40)             NOT NULL
CCC004*     ,HZ_TYP               CHAR(8)              NOT NULL
CCC004*     ,HZ_PG                SMALLINT             NOT NULL
CCC004*     ,HZ_VB                SMALLINT             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3637
        01 OVPROD-EINB-REF00-SATZ.
         10 PG                                PIC S9(4) COMP.
         10 PRODSCHL                          PIC S9(4) COMP.
         10 SEL-WERT                          PIC X(15).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 PROD-KURZ                         PIC X(10).
         10 PROD-LANG                         PIC X(40).
         10 HZ-TYP                            PIC X(8).
         10 HZ-PG                             PIC S9(4) COMP.
         10 HZ-VB                             PIC S9(4) COMP.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

