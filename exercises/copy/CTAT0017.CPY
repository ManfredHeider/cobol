      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:49:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTAT0017                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBAT0017 (PGES)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:18:47
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !23.12.94! 01 ! ANPASSUNG NEUES FORMAT    ! KUHN     *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBAT0017)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0017 TABLE
CCC004*    ( KNR_PROD_GB_GES                SMALLINT NOT NULL,
CCC004*      PGES_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      PGES_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGES_DAT_BIS                   DATE      NOT NULL,
CCC004*      KNR_AUSL                       SMALLINT NOT NULL,
CCC004*      KNR_BEWERT_ART                 SMALLINT NOT NULL,
CCC004*      KTY_PROD_GB_GES                SMALLINT NOT NULL,
CCC004*      KTY_AUSL                       SMALLINT NOT NULL,
CCC004*      KTY_BEWERT_ART                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0017                           *
      ******************************************************************
       01  TBAT0017-GESAMT.
        05  NI-TBAT0017-TAB.
           10 NI-TBAT0017-ELE      PIC S9(04) COMP  OCCURS 9 TIMES.
        05  NI-TBAT0017            REDEFINES  NI-TBAT0017-TAB.
           10 NI-KNR-PROD-GB-GES   PIC S9(4) USAGE COMP.
           10 NI-PGES-DAT-VON      PIC S9(4) USAGE COMP.
           10 NI-PGES-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-PGES-DAT-BIS      PIC S9(4) USAGE COMP.
           10 NI-KNR-AUSL          PIC S9(4) USAGE COMP.
           10 NI-KNR-BEWERT-ART    PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-GB-GES   PIC S9(4) USAGE COMP.
           10 NI-KTY-AUSL          PIC S9(4) USAGE COMP.
           10 NI-KTY-BEWERT-ART    PIC S9(4) USAGE COMP.
        05  DCL-TBAT0017.
           10 KNR-PROD-GB-GES      PIC S9(4) USAGE COMP.
           10 PGES-DAT-VON         PIC  X(26).
           10 PGES-LAST-UPDATE     PIC  X(26).
           10 PGES-DAT-BIS         PIC  X(10).
           10 KNR-AUSL             PIC S9(4) USAGE COMP.
           10 KNR-BEWERT-ART       PIC S9(4) USAGE COMP.
           10 KTY-PROD-GB-GES      PIC S9(4) USAGE COMP.
           10 KTY-AUSL             PIC S9(4) USAGE COMP.
           10 KTY-BEWERT-ART       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
