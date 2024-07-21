      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.04.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0230.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0230.CPv  $
      *
      *   Rev 2.1   Jun 19 2018 13:45:52   T13EX18
      *ASL00583: Einührung Allfinanz AG in VB-System / Geldleistung
      *en Zugriffsmodul FPRC (GBU)
      *
      *   Rev 2.0   Apr 13 2018 17:42:50   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00583!13.04.18!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0230) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0230)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0230 TABLE
CCC004*    ( FPRC_ST_NR                  INTEGER NOT NULL
CCC004*    , FPRC_STICHTAG               DATE NOT NULL
CCC004*    , FPRC_ANSPR_VOM              DATE NOT NULL
CCC004*    , FPRC_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , FPRC_MS_NR                  SMALLINT NOT NULL
CCC004*    , FPRC_12M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_12M_EQF                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_03M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EQF_ABZ_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRC_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_ANZ_MONATE             SMALLINT NOT NULL
CCC004*    , FPRC_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EQF_ABZ_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRC_EH_ABZ_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_KORR_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_KORR_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_ANT_FREI            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_EH_ANT_UPFL            DECIMAL(05, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0230                           *
      *****************************************************************
 
       01  VIVB0230-GESAMT.
        05 NI-VIVB0230-TAB.
           10 NI-VIVB0230-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVB0230             REDEFINES   NI-VIVB0230-TAB.
           10 NI-FPRC-ST-NR               PIC S9(04)       BINARY.
           10 NI-FPRC-STICHTAG            PIC S9(04)       BINARY.
           10 NI-FPRC-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-FPRC-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-FPRC-MS-NR               PIC S9(04)       BINARY.
           10 NI-FPRC-12M-UMS-FREI        PIC S9(04)       BINARY.
           10 NI-FPRC-12M-EQF             PIC S9(04)       BINARY.
           10 NI-FPRC-03M-UMS-FREI        PIC S9(04)       BINARY.
           10 NI-FPRC-EQF-ABZ-FREI        PIC S9(04)       BINARY.
           10 NI-FPRC-FAKTOR              PIC S9(04)       BINARY.
           10 NI-FPRC-ANZ-MONATE          PIC S9(04)       BINARY.
           10 NI-FPRC-12M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRC-03M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRC-EQF-ABZ-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRC-EH-ABZ-NEG          PIC S9(04)       BINARY.
           10 NI-FPRC-03M-KORR-FREI       PIC S9(04)       BINARY.
           10 NI-FPRC-03M-KORR-UPFL       PIC S9(04)       BINARY.
           10 NI-FPRC-EH-ANT-FREI         PIC S9(04)       BINARY.
           10 NI-FPRC-EH-ANT-UPFL         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0230.
           10 FPRC-ST-NR                  PIC S9(09)       BINARY.
           10 FPRC-STICHTAG               PIC  X(10).
           10 FPRC-ANSPR-VOM              PIC  X(10).
           10 FPRC-LAST-UPDATE            PIC  X(26).
           10 FPRC-MS-NR                  PIC S9(04)       BINARY.
           10 FPRC-12M-UMS-FREI           PIC S9(09)V9(01) COMP-3.
           10 FPRC-12M-EQF                PIC S9(03)V9(02) COMP-3.
           10 FPRC-03M-UMS-FREI           PIC S9(09)V9(01) COMP-3.
           10 FPRC-EQF-ABZ-FREI           PIC S9(09)V9(02) COMP-3.
           10 FPRC-FAKTOR                 PIC S9(03)V9(02) COMP-3.
           10 FPRC-ANZ-MONATE             PIC S9(04)       BINARY.
           10 FPRC-12M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 FPRC-03M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 FPRC-EQF-ABZ-UPFL           PIC S9(09)V9(02) COMP-3.
           10 FPRC-EH-ABZ-NEG             PIC S9(09)V9(01) COMP-3.
           10 FPRC-03M-KORR-FREI          PIC S9(09)V9(01) COMP-3.
           10 FPRC-03M-KORR-UPFL          PIC S9(09)V9(01) COMP-3.
           10 FPRC-EH-ANT-FREI            PIC S9(03)V9(02) COMP-3.
           10 FPRC-EH-ANT-UPFL            PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

