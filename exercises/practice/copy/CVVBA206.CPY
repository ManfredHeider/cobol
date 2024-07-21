      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA206.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA206.CPv  $
      *
      *   Rev 2.1   Jun 01 2017 14:48:42   t13ae30
      *ASL00567: Anpassen QED Ermittlung - TWI
      *
      *   Rev 2.0   Mar 30 2017 14:21:40   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    06.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00567!06.04.17!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA206) QEDA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA206)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA206 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , ED_ST_NR                    INTEGER NOT NULL
CCC004*    , QEDA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , QEDA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , QEDA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , QEDA_ANSPR_BETR_UST         DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA206                           *
      *****************************************************************

       01  VIVBA206-GESAMT.
        05 NI-VIVBA206-TAB.
           10 NI-VIVBA206-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVBA206             REDEFINES   NI-VIVBA206-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-ED-ST-NR                 PIC S9(04)       BINARY.
           10 NI-QEDA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-QEDA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-QEDA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-QEDA-ANSPR-BETR-UST      PIC S9(04)       BINARY.

        05 DCL-VIVBA206.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 ED-ST-NR                    PIC S9(09)       BINARY.
           10 QEDA-ANSPR-VOM              PIC  X(10).
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 QEDA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 QEDA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 QEDA-ANSPR-BETR-UST         PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

