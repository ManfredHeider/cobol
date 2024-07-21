      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0229.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0229.CPv  $
      *
      *   Rev 2.1   May 02 2017 16:33:24   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0229
      *
      *   Rev 2.0   May 02 2017 10:58:30   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0229) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0229)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0229 TABLE
CCC004*    ( FPRB_ST_NR                  INTEGER NOT NULL
CCC004*    , FPRB_STICHTAG               DATE NOT NULL
CCC004*    , FPRB_ANSPR_VOM              DATE NOT NULL
CCC004*    , FPRB_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , FPRB_MS_NR                  SMALLINT NOT NULL
CCC004*    , FPRB_12M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_12M_EQF                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_03M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EQF_ABZ_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRB_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_ANZ_MONATE             SMALLINT NOT NULL
CCC004*    , FPRB_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EQF_ABZ_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRB_EH_ABZ_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_KORR_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_KORR_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_ANT_FREI            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_EH_ANT_UPFL            DECIMAL(05, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0229                           *
      *****************************************************************
 
       01  VIVB0229-GESAMT.
        05 NI-VIVB0229-TAB.
           10 NI-VIVB0229-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVB0229             REDEFINES   NI-VIVB0229-TAB.
           10 NI-FPRB-ST-NR               PIC S9(04)       BINARY.
           10 NI-FPRB-STICHTAG            PIC S9(04)       BINARY.
           10 NI-FPRB-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-FPRB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-FPRB-MS-NR               PIC S9(04)       BINARY.
           10 NI-FPRB-12M-UMS-FREI        PIC S9(04)       BINARY.
           10 NI-FPRB-12M-EQF             PIC S9(04)       BINARY.
           10 NI-FPRB-03M-UMS-FREI        PIC S9(04)       BINARY.
           10 NI-FPRB-EQF-ABZ-FREI        PIC S9(04)       BINARY.
           10 NI-FPRB-FAKTOR              PIC S9(04)       BINARY.
           10 NI-FPRB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 NI-FPRB-12M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRB-03M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRB-EQF-ABZ-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRB-EH-ABZ-NEG          PIC S9(04)       BINARY.
           10 NI-FPRB-03M-KORR-FREI       PIC S9(04)       BINARY.
           10 NI-FPRB-03M-KORR-UPFL       PIC S9(04)       BINARY.
           10 NI-FPRB-EH-ANT-FREI         PIC S9(04)       BINARY.
           10 NI-FPRB-EH-ANT-UPFL         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0229.
           10 FPRB-ST-NR                  PIC S9(09)       BINARY.
           10 FPRB-STICHTAG               PIC  X(10).
           10 FPRB-ANSPR-VOM              PIC  X(10).
           10 FPRB-LAST-UPDATE            PIC  X(26).
           10 FPRB-MS-NR                  PIC S9(04)       BINARY.
           10 FPRB-12M-UMS-FREI           PIC S9(09)V9(01) COMP-3.
           10 FPRB-12M-EQF                PIC S9(03)V9(02) COMP-3.
           10 FPRB-03M-UMS-FREI           PIC S9(09)V9(01) COMP-3.
           10 FPRB-EQF-ABZ-FREI           PIC S9(09)V9(02) COMP-3.
           10 FPRB-FAKTOR                 PIC S9(03)V9(02) COMP-3.
           10 FPRB-ANZ-MONATE             PIC S9(04)       BINARY.
           10 FPRB-12M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 FPRB-03M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 FPRB-EQF-ABZ-UPFL           PIC S9(09)V9(02) COMP-3.
           10 FPRB-EH-ABZ-NEG             PIC S9(09)V9(01) COMP-3.
           10 FPRB-03M-KORR-FREI          PIC S9(09)V9(01) COMP-3.
           10 FPRB-03M-KORR-UPFL          PIC S9(09)V9(01) COMP-3.
           10 FPRB-EH-ANT-FREI            PIC S9(03)V9(02) COMP-3.
           10 FPRB-EH-ANT-UPFL            PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

