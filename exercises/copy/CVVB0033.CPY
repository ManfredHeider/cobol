      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:18:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVB0033                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVB0033 (KTO)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    15.09.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    29.10.94 10:22:39
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!15.09.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIVB0033)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0033 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      EV_MONAT                       DATE NOT NULL,
CCC004*      KNR_VERGUET_TYP                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_VERGUET_TYP                SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      EV_EQF                         DECIMAL(5, 2) NOT NULL,
CCC004*      EV_STORMIND_PROZ               DECIMAL(5, 2) NOT NULL,
CCC004*      EV_AE_MONATS_DS                DECIMAL(10, 1) NOT NULL,
CCC004*      EV_AUSZAHLUNG                  DECIMAL(11, 2) NOT NULL,
CCC004*      EV_BRUTTO_BETRAG               DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0033                           *
      ******************************************************************
       01  VIVB0033-GESAMT.
        05  NI-VIVB0033-TAB.
           10 NI-VIVB0033-ELE      PIC S9(04) COMP  OCCURS 11 TIMES.
        05  NI-VIVB0033            REDEFINES  NI-VIVB0033-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-EV-MONAT          PIC S9(4) USAGE COMP.
           10 NI-KNR-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTY-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-EV-EQF            PIC S9(4) USAGE COMP.
           10 NI-EV-STORMIND-PROZ  PIC S9(4) USAGE COMP.
           10 NI-EV-AE-MONATS-DS   PIC S9(4) USAGE COMP.
           10 NI-EV-AUSZAHLUNG     PIC S9(4) USAGE COMP.
           10 NI-EV-BRUTTO-BETRAG  PIC S9(4) USAGE COMP.
        05  DCL-VIVB0033.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 EV-MONAT             PIC X(10).
           10 KNR-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 EV-EQF               PIC S999V99 USAGE COMP-3.
           10 EV-STORMIND-PROZ     PIC S999V99 USAGE COMP-3.
           10 EV-AE-MONATS-DS      PIC S999999999V9 USAGE COMP-3.
           10 EV-AUSZAHLUNG        PIC S999999999V99 USAGE COMP-3.
           10 EV-BRUTTO-BETRAG     PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
