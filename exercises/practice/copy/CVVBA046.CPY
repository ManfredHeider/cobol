      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA046.CPv  $
      *
      *   Rev 2.1   Apr 20 2012 08:20:46   T13EX43
      *ASL00419: PGM - KLZ für Central-Wechsler
      *
      *   Rev 2.0   Mar 06 2012 12:29:50   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    06.03.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00419!06.03.12!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA046) KLZA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA046)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA046 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , KLZA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KLZA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KLZA_BEITRAG                DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KLZA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KNR_ANSPR_STATUS            SMALLINT
CCC004*    , KLZA_CENTRAL_KZ             CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA046                           *
      *****************************************************************

       01  VIVBA046-GESAMT.
        05 NI-VIVBA046-TAB.
           10 NI-VIVBA046-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVBA046             REDEFINES   NI-VIVBA046-TAB.
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
           10 NI-KLZA-CENTRAL-KZ          PIC S9(04)       BINARY.

        05 DCL-VIVBA046.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KLZA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KLZA-LAST-UPDATE            PIC  X(26).
           10 KLZA-BEITRAG                PIC S9(09)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 KLZA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KLZA-CENTRAL-KZ             PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************
