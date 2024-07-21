      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVATA017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVATA017.CPv  $
      *
      *   Rev 2.1   Mar 31 2000 17:50:18   T13AE34
      *AAN00249-SF21 Änderung
      *
      *   Rev 2.0   Mar 28 2000 09:03:36   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVATA017                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIATA017 (PGES)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:20:23
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !23.12.94! 01 ! ANPASSUNG NEUES FORMAT    ! KUHN     *  00145000
      * AAN00249!15.03.00!2.1 ! ZUSAMMENLEGEN MIT TBPA0008! HEIDER   *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIAT0017)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIATA017 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PGEB_GUELTIG_AB                TIMESTAMP NOT NULL,
CCC004*      PGEB_GUELTIG_BIS               DATE      NOT NULL,
CCC004*      KTY_PGEB_GES                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_EXPORT_ART                 SMALLINT NOT NULL,
CCC004*      KNR_EXPORT_ART                 SMALLINT NOT NULL,
CCC004*      KTY_VERTRIEB_GES               SMALLINT NOT NULL,
CCC004*      VERTRIEB_GES                   SMALLINT NOT NULL,
CCC004*      KTY_AUSL                       SMALLINT NOT NULL,
CCC004*      KNR_AUSL                       SMALLINT NOT NULL,
CCC004*      KTY_BEWERT_ART                 SMALLINT NOT NULL,
CCC004*      KNR_BEWERT_ART                 SMALLINT NOT NULL,
CCC004*      PGEB_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGEB_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.

       01  DCL-TBAT0017.
           03 PGEB-GES                       PIC S9(4) COMP.
           03 PGEB-GUELTIG-AB                PIC X(26).
           03 PGEB-GUELTIG-BIS               PIC X(10).
           03 KTY-PGEB-GES                   PIC S9(4) COMP.
           03 KTY-WAEHRUNG                   PIC S9(4) COMP.
           03 KNR-WAEHRUNG                   PIC S9(4) COMP.
           03 KTY-EXPORT-ART                 PIC S9(4) COMP.
           03 KNR-EXPORT-ART                 PIC S9(4) COMP.
           03 KTY-VERTRIEB-GES               PIC S9(4) COMP.
           03 VERTRIEB-GES                   PIC S9(4) COMP.
           03 KTY-AUSL                       PIC S9(4) COMP.
           03 KNR-AUSL                       PIC S9(4) COMP.
           03 KTY-BEWERT-ART                 PIC S9(4) COMP.
           03 KNR-BEWERT-ART                 PIC S9(4) COMP.
           03 PGEB-LAST-UPDATE               PIC X(26).
           03 PGEB-KENNUNG                   PIC X(8).
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA017                           *
      ******************************************************************
