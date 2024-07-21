      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0076.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVVB0076.CPv  $
      *
      *   Rev 2.1   Apr 25 2000 15:44:34   T13AE35
      *ASL00049 - neu fuer Tabelle TBVB0076
      *im Rahmen DIM7
      *
      *   Rev 2.0   Feb 18 2000 10:15:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVB0076                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVB0076 (OBAZ)                                 * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: Walb                                             * 00060002
      *    DATUM:    12.04.2000                                       * 00070002
      ***************************************************************** 00080000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * ASL00049!12.04.00! 01 ! NEU                       ! Walb     *  00140002
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIVB0076)                                         *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0076 TABLE
CCC004*    ( OBAZ_ST_NR                     INTEGER NOT NULL,
CCC004*      OBAZ_ANSPR_VOM                 DATE NOT NULL,
CCC004*      OBAZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      OBAZ_ANSPR_BETRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_ANSPR_STATUS               SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS               SMALLINT NOT NULL
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0076                           *
      ******************************************************************
       01  VIVB0076-GESAMT.
        05  NI-VIVB0076-TAB.
           10  NI-VIVB0076-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-VIVB0076            REDEFINES   NI-VIVB0076-TAB.
           10 NI-OBAZ-ST-NR        PIC S9(04)  COMP.
           10 NI-OBAZ-ANSPR-VOM    PIC S9(04)  COMP.
           10 NI-OBAZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(04)  COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(04)  COMP.
           10 NI-OBAZ-ANSPR-BETRAG PIC S9(04)  COMP.
           10 NI-KTY-ANSPR-STATUS  PIC S9(04)  COMP.
           10 NI-KNR-ANSPR-STATUS  PIC S9(04)  COMP.
        05  DCL-VIVB0076.
           10 OBAZ-ST-NR           PIC S9(9) USAGE COMP.
           10 OBAZ-ANSPR-VOM       PIC X(10).
           10 OBAZ-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 OBAZ-ANSPR-BETRAG    PIC S999999999V99 USAGE COMP-3.
           10 KTY-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 KNR-ANSPR-STATUS     PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

