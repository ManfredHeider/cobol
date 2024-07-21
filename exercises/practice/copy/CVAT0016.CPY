      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:08:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVAT0016                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIAT0016 (PRPS)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:19:44
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !23.12.94! 01 ! ANPASSUNG NEUES FORMAT    ! KUHN     *  00140000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIAT0016)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0016 TABLE
CCC004*    ( KNR_PROD_GB_GES                SMALLINT NOT NULL,
CCC004*      PRPS_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PRPS_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      PRPS_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PRPS_DAT_BIS                   DATE NOT NULL,
CCC004*      KNR_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KNR_ART_PROV_TYP               SMALLINT NOT NULL,
CCC004*      KTY_PROD_GB_GES                SMALLINT NOT NULL,
CCC004*      KTY_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KTY_ART_PROV_TYP               SMALLINT NOT NULL,
CCC004*      PRPS_PROD_BEZ                  CHAR (40) NOT NULL,
CCC004*      PRPS_PROD_BEZ10                CHAR (10) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0016                           *
      ******************************************************************
       01  VIAT0016-GESAMT.
        05  NI-VIAT0016-TAB.
           10 NI-VIAT0016-ELE      PIC S9(04) COMP  OCCURS 12 TIMES.
        05  NI-VIAT0016            REDEFINES  NI-VIAT0016-TAB.
           10 NI-KNR-PROD-GB-GES   PIC S9(4) USAGE COMP.
           10 NI-PRPS-PROD-SCHL    PIC S9(4) USAGE COMP.
           10 NI-PRPS-DAT-VON      PIC S9(4) USAGE COMP.
           10 NI-PRPS-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-PRPS-DAT-BIS      PIC S9(4) USAGE COMP.
           10 NI-KNR-PROD-SPARTE   PIC S9(4) USAGE COMP.
           10 NI-KNR-ART-PROV-TYP  PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-GB-GES   PIC S9(4) USAGE COMP.
           10 NI-KTY-PROD-SPARTE   PIC S9(4) USAGE COMP.
           10 NI-KTY-ART-PROV-TYP  PIC S9(4) USAGE COMP.
           10 NI-PRPS-PROD-BEZ     PIC S9(4) USAGE COMP.
           10 NI-PRPS-PROD-BEZ10   PIC S9(4) USAGE COMP.
        05  DCL-VIAT0016.
           10 KNR-PROD-GB-GES      PIC S9(4) USAGE COMP.
           10 PRPS-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PRPS-DAT-VON         PIC  X(26).
           10 PRPS-LAST-UPDATE     PIC  X(26).
           10 PRPS-DAT-BIS         PIC  X(10).
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KNR-ART-PROV-TYP     PIC S9(4) USAGE COMP.
           10 KTY-PROD-GB-GES      PIC S9(4) USAGE COMP.
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KTY-ART-PROV-TYP     PIC S9(4) USAGE COMP.
           10 PRPS-PROD-BEZ        PIC  X(40).
           10 PRPS-PROD-BEZ10      PIC  X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
