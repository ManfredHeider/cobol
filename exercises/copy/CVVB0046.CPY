      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0046.CPv  $
      *
      *   Rev 2.1   Jun 02 2005 16:43:58   t13ex43
      *ASL00204: PGM - K-Leistungszuschuss Zugriffsmodule
      *
      *   Rev 2.0   Jun 01 2005 14:28:52   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    02.06.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00204!02.06.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0046) KLZA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0046)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0046 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      KLZA_ANSPR_VOM              DATE NOT NULL,
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KLZA_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      KLZA_BEITRAG                DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KLZA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0046
      ******************************************************************
       01  VIVB0046-GESAMT.
        05  NI-VIVB0046-TAB.
           10 NI-VIVB0046-ELE      PIC S9(04) BINARY OCCURS  11 TIMES.
        05  NI-VIVB0046            REDEFINES   NI-VIVB0046-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-KLZA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KLZA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KLZA-BEITRAG             PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KLZA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05  DCL-VIVB0046.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KLZA-ANSPR-VOM              PIC X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KLZA-LAST-UPDATE            PIC X(26).
           10 KLZA-BEITRAG                PIC S9(09)V9(02) USAGE COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 KLZA-ANSPR-BETRAG           PIC S9(09)V9(02) USAGE COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
