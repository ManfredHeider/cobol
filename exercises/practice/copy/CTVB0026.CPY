      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:56:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0026                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0026 (OLZZ)                                 * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060002
      *    DATUM:    11.09.1992                                       * 00070002
      ***************************************************************** 00080000
      *    VERSION   1    23.02.95 15:10:18
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      *         !11.09.92! 01 ! NEU                       ! KUHN     *  00140002
      * AFVB0183!03.02.95! 02 ! OLZZ NEU STRUKTURIERT     ! SCHÖLLH. *  00140002
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
030295******************************************************************
030295* DCLGEN TABLE(TBVB0026)                                         *
030295* ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
030295******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0026 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      OLZZ_MONAT                     DATE NOT NULL,
CCC004*      KNR_OLZ_TYP                    SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      OLZZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KTY_OLZ_TYP                    SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      OLZZ_BETRAG                    DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
030295******************************************************************
030295* COBOL DECLARATION FOR TABLE TBVB0026                           *
030295******************************************************************
030295 01  TBVB0026-GESAMT.
030295  05  NI-TBVB0026-TAB.
030295     10  NI-TBVB0026-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
030295  05  NI-TBVB0026            REDEFINES   NI-TBVB0026-TAB.
030295     10 NI-MA-ST-NR          PIC S9(04)  COMP.
030295     10 NI-OLZZ-MONAT        PIC S9(04)  COMP.
030295     10 NI-KNR-OLZ-TYP       PIC S9(04)  COMP.
030295     10 NI-KNR-WAEHRUNG      PIC S9(04)  COMP.
030295     10 NI-OLZZ-LAST-UPDATE  PIC S9(04)  COMP.
030295     10 NI-KTY-OLZ-TYP       PIC S9(04)  COMP.
030295     10 NI-KTY-WAEHRUNG      PIC S9(04)  COMP.
030295     10 NI-OLZZ-BETRAG       PIC S9(04)  COMP.
030295  05  DCL-TBVB0026.
030295     10 MA-ST-NR             PIC S9(9) USAGE COMP.
030295     10 OLZZ-MONAT           PIC X(10).
030295     10 KNR-OLZ-TYP          PIC S9(4) USAGE COMP.
030295     10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
030295     10 OLZZ-LAST-UPDATE     PIC X(26).
030295     10 KTY-OLZ-TYP          PIC S9(4) USAGE COMP.
030295     10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
030295     10 OLZZ-BETRAG          PIC S999999999V99 USAGE COMP-3.
030295******************************************************************
030295* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
030295******************************************************************

030295******************************************************************
030295* DCLGEN TABLE(TBVB0026)                                         *
030295* ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
030295******************************************************************
030295*    EXEC SQL DECLARE TBVB0026 TABLE
030295*    ( MA_ST_NR                       INTEGER NOT NULL,
030295*      OLZZ_QUARTAL                   DATE NOT NULL,
030295*      OLZZ_LAST_UPDATE               TIMESTAMP NOT NULL,
030295*      OLZZ_BUERO                     DECIMAL(11, 2) NOT NULL,
030295*      OLZZ_LEISTUNG                  DECIMAL(11, 2) NOT NULL,
030295*      OLZZ_ORG                       DECIMAL(11, 2) NOT NULL
030295*    ) END-EXEC.
030295******************************************************************
030295* COBOL DECLARATION FOR TABLE TBVB0026                           *
030295******************************************************************
030295*01  TBVB0026-GESAMT.
030295* 05  NI-TBVB0026-TAB.
030295*    10  NI-TBVB0026-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
030295* 05  NI-TBVB0026            REDEFINES   NI-TBVB0026-TAB.
030295*    10 NI-MA-ST-NR          PIC S9(04)  COMP.
030295*    10 NI-OLZZ-QUARTAL      PIC S9(04)  COMP.
030295*    10 NI-OLZZ-LAST-UPDATE  PIC S9(04)  COMP.
030295*    10 NI-OLZZ-BUERO        PIC S9(04)  COMP.
030295*    10 NI-OLZZ-LEISTUNG     PIC S9(04)  COMP.
030295*    10 NI-OLZZ-ORG          PIC S9(04)  COMP.
030295* 05  DCL-TBVB0026.
030295*    10 MA-ST-NR             PIC S9(9) USAGE COMP.
030295*    10 OLZZ-QUARTAL         PIC X(10).
030295*    10 OLZZ-LAST-UPDATE     PIC X(26).
030295*    10 OLZZ-BUERO           PIC S999999999V99 USAGE COMP-3.
030295*    10 OLZZ-LEISTUNG        PIC S999999999V99 USAGE COMP-3.
030295*    10 OLZZ-ORG             PIC S999999999V99 USAGE COMP-3.
030295******************************************************************
030295* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
030295******************************************************************
