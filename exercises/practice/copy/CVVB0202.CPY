      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0202.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:11:04   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 13:43:40   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: OBER                                             *
      *    DATUM:    15.07.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00352!15.07.10!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0202) BOZBALT                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0202)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0202 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , STICHTAG                    DATE NOT NULL
CCC004*    , ANSPR_MONAT                 DATE NOT NULL
CCC004*    , ANSPR_VOM                   DATE NOT NULL
CCC004*    , BOZBALT_LAST_UPD            TIMESTAMP NOT NULL
CCC004*    , MS_NR                       SMALLINT NOT NULL
CCC004*    , BOZ_12MON_UMS               DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_12MON_EQF               DECIMAL(05, 2) NOT NULL
CCC004*    , BOZ_03MON_UMS               DECIMAL(10, 1) NOT NULL
CCC004*    , EH_PRO_ABZUG                DECIMAL(10, 1) NOT NULL
CCC004*    , EH_NACH_PRO_ABZUG           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_ZW_BETRAG               DECIMAL(11, 2) NOT NULL
CCC004*    , EQF_ABZUG                   DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_BETRAG                  DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_FAKTOR                  DECIMAL(05, 2) NOT NULL
CCC004*    , BOZ_ANZ_MONATE              SMALLINT NOT NULL
CCC004*    , UMS_BED_ERF                 CHAR(01) NOT NULL
CCC004*    , BOZ_ALT_AUSZAHLUNG          CHAR(01) NOT NULL
CCC004*    , BOZ_NACH_DIR                CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0202                           *
      *****************************************************************

       01  VIVB0202-GESAMT.
        05 NI-VIVB0202-TAB.
           10 NI-VIVB0202-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVB0202             REDEFINES   NI-VIVB0202-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-STICHTAG                 PIC S9(04)       BINARY.
           10 NI-ANSPR-MONAT              PIC S9(04)       BINARY.
           10 NI-ANSPR-VOM                PIC S9(04)       BINARY.
           10 NI-BOZBALT-LAST-UPD         PIC S9(04)       BINARY.
           10 NI-MS-NR                    PIC S9(04)       BINARY.
           10 NI-BOZ-12MON-UMS            PIC S9(04)       BINARY.
           10 NI-BOZ-12MON-EQF            PIC S9(04)       BINARY.
           10 NI-BOZ-03MON-UMS            PIC S9(04)       BINARY.
           10 NI-EH-PRO-ABZUG             PIC S9(04)       BINARY.
           10 NI-EH-NACH-PRO-ABZUG        PIC S9(04)       BINARY.
           10 NI-BOZ-ZW-BETRAG            PIC S9(04)       BINARY.
           10 NI-EQF-ABZUG                PIC S9(04)       BINARY.
           10 NI-BOZ-BETRAG               PIC S9(04)       BINARY.
           10 NI-BOZ-FAKTOR               PIC S9(04)       BINARY.
           10 NI-BOZ-ANZ-MONATE           PIC S9(04)       BINARY.
           10 NI-UMS-BED-ERF              PIC S9(04)       BINARY.
           10 NI-BOZ-ALT-AUSZAHLUNG       PIC S9(04)       BINARY.
           10 NI-BOZ-NACH-DIR             PIC S9(04)       BINARY.

        05 DCL-VIVB0202.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 STICHTAG                    PIC  X(10).
           10 ANSPR-MONAT                 PIC  X(10).
           10 ANSPR-VOM                   PIC  X(10).
           10 BOZBALT-LAST-UPD            PIC  X(26).
           10 MS-NR                       PIC S9(04)       BINARY.
           10 BOZ-12MON-UMS               PIC S9(09)V9(01) COMP-3.
           10 BOZ-12MON-EQF               PIC S9(03)V9(02) COMP-3.
           10 BOZ-03MON-UMS               PIC S9(09)V9(01) COMP-3.
           10 EH-PRO-ABZUG                PIC S9(09)V9(01) COMP-3.
           10 EH-NACH-PRO-ABZUG           PIC S9(09)V9(01) COMP-3.
           10 BOZ-ZW-BETRAG               PIC S9(09)V9(02) COMP-3.
           10 EQF-ABZUG                   PIC S9(09)V9(02) COMP-3.
           10 BOZ-BETRAG                  PIC S9(09)V9(02) COMP-3.
           10 BOZ-FAKTOR                  PIC S9(03)V9(02) COMP-3.
           10 BOZ-ANZ-MONATE              PIC S9(04)       BINARY.
           10 UMS-BED-ERF                 PIC  X(01).
           10 BOZ-ALT-AUSZAHLUNG          PIC  X(01).
           10 BOZ-NACH-DIR                PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

