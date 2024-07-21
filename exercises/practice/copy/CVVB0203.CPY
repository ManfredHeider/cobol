      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0203.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0203.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:11:20   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 14:07:16   t13ae14
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
      * DCLGEN TABLE(VIVB0203) BOZBNEU                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0203)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0203 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , STICHTAG                    DATE NOT NULL
CCC004*    , ANSPR_MONAT                 DATE NOT NULL
CCC004*    , ANSPR_VOM                   DATE NOT NULL
CCC004*    , BOZBNEU_LAST_UPD            TIMESTAMP NOT NULL
CCC004*    , UMS_12_EIG                  DECIMAL(10, 1) NOT NULL
CCC004*    , UMS_12_GRU                  DECIMAL(10, 1) NOT NULL
CCC004*    , UMS_12_GES                  DECIMAL(10, 1) NOT NULL
CCC004*    , EQF_12_GRU                  DECIMAL(05, 2) NOT NULL
CCC004*    , UMS_03_GRU                  DECIMAL(10, 1) NOT NULL
CCC004*    , EH_PRO_ABZUG                DECIMAL(10, 1) NOT NULL
CCC004*    , EH_NACH_PRO_ABZUG           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_BASIS_BETRAG            DECIMAL(11, 2) NOT NULL
CCC004*    , UMS_03_PART                 DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_AUFBAU_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_ZW_SUMME                DECIMAL(11, 2) NOT NULL
CCC004*    , EQF_ABZUG                   DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_BETRAG                  DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_FAKTOR_BASIS            DECIMAL(05, 2) NOT NULL
CCC004*    , BOZ_FAKTOR_AUFBAU           DECIMAL(05, 2) NOT NULL
CCC004*    , BOZ_ANZ_MONATE              SMALLINT NOT NULL
CCC004*    , BOZ_NEU_AUSZAHLUNG          CHAR(01) NOT NULL
CCC004*    , BOZ_NACH_DIR                CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0203                           *
      *****************************************************************

       01  VIVB0203-GESAMT.
        05 NI-VIVB0203-TAB.
           10 NI-VIVB0203-ELE      PIC S9(04)  BINARY OCCURS  23.
        05 NI-VIVB0203             REDEFINES   NI-VIVB0203-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-STICHTAG                 PIC S9(04)       BINARY.
           10 NI-ANSPR-MONAT              PIC S9(04)       BINARY.
           10 NI-ANSPR-VOM                PIC S9(04)       BINARY.
           10 NI-BOZBNEU-LAST-UPD         PIC S9(04)       BINARY.
           10 NI-UMS-12-EIG               PIC S9(04)       BINARY.
           10 NI-UMS-12-GRU               PIC S9(04)       BINARY.
           10 NI-UMS-12-GES               PIC S9(04)       BINARY.
           10 NI-EQF-12-GRU               PIC S9(04)       BINARY.
           10 NI-UMS-03-GRU               PIC S9(04)       BINARY.
           10 NI-EH-PRO-ABZUG             PIC S9(04)       BINARY.
           10 NI-EH-NACH-PRO-ABZUG        PIC S9(04)       BINARY.
           10 NI-BOZ-BASIS-BETRAG         PIC S9(04)       BINARY.
           10 NI-UMS-03-PART              PIC S9(04)       BINARY.
           10 NI-BOZ-AUFBAU-BETRAG        PIC S9(04)       BINARY.
           10 NI-BOZ-ZW-SUMME             PIC S9(04)       BINARY.
           10 NI-EQF-ABZUG                PIC S9(04)       BINARY.
           10 NI-BOZ-BETRAG               PIC S9(04)       BINARY.
           10 NI-BOZ-FAKTOR-BASIS         PIC S9(04)       BINARY.
           10 NI-BOZ-FAKTOR-AUFBAU        PIC S9(04)       BINARY.
           10 NI-BOZ-ANZ-MONATE           PIC S9(04)       BINARY.
           10 NI-BOZ-NEU-AUSZAHLUNG       PIC S9(04)       BINARY.
           10 NI-BOZ-NACH-DIR             PIC S9(04)       BINARY.

        05 DCL-VIVB0203.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 STICHTAG                    PIC  X(10).
           10 ANSPR-MONAT                 PIC  X(10).
           10 ANSPR-VOM                   PIC  X(10).
           10 BOZBNEU-LAST-UPD            PIC  X(26).
           10 UMS-12-EIG                  PIC S9(09)V9(01) COMP-3.
           10 UMS-12-GRU                  PIC S9(09)V9(01) COMP-3.
           10 UMS-12-GES                  PIC S9(09)V9(01) COMP-3.
           10 EQF-12-GRU                  PIC S9(03)V9(02) COMP-3.
           10 UMS-03-GRU                  PIC S9(09)V9(01) COMP-3.
           10 EH-PRO-ABZUG                PIC S9(09)V9(01) COMP-3.
           10 EH-NACH-PRO-ABZUG           PIC S9(09)V9(01) COMP-3.
           10 BOZ-BASIS-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 UMS-03-PART                 PIC S9(09)V9(01) COMP-3.
           10 BOZ-AUFBAU-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 BOZ-ZW-SUMME                PIC S9(09)V9(02) COMP-3.
           10 EQF-ABZUG                   PIC S9(09)V9(02) COMP-3.
           10 BOZ-BETRAG                  PIC S9(09)V9(02) COMP-3.
           10 BOZ-FAKTOR-BASIS            PIC S9(03)V9(02) COMP-3.
           10 BOZ-FAKTOR-AUFBAU           PIC S9(03)V9(02) COMP-3.
           10 BOZ-ANZ-MONATE              PIC S9(04)       BINARY.
           10 BOZ-NEU-AUSZAHLUNG          PIC  X(01).
           10 BOZ-NACH-DIR                PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************
