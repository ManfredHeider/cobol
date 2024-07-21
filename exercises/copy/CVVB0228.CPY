      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0228.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0228.CPv  $
      *
      *   Rev 2.1   May 02 2017 16:29:08   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0228
      *
      *   Rev 2.0   May 02 2017 11:00:28   t13ex18
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
      * DCLGEN TABLE(VIVB0228) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0228)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0228 TABLE
CCC004*    ( FPRDD_ST_NR                 INTEGER NOT NULL
CCC004*    , FPRDD_STICHTAG              DATE NOT NULL
CCC004*    , FPRDD_ANSPR_MONAT           DATE NOT NULL
CCC004*    , FPRDD_ANSPR_VOM             DATE NOT NULL
CCC004*    , FPRDD_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , FPRDD_MS_NR                 SMALLINT NOT NULL
CCC004*    , FPRDD_12M_UMS_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_12M_EQF               DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_03M_UMS_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_PRO_ABZUG_FREI        DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_N_PRO_ABZ_FREI        DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ZW_BETRAG             DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_EQF_ABZUG_FREI        DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_BETRAG_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_FAKTOR                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_ANZ_MONATE            SMALLINT NOT NULL
CCC004*    , FPRDD_UMS_BED_ERF           CHAR(01) NOT NULL
CCC004*    , FPRDD_S_AUSZAHLUNG          CHAR(01) NOT NULL
CCC004*    , FPRDD_S_NACH_DIR            CHAR(01) NOT NULL
CCC004*    , FPRDD_12M_UMS_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_UMS_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_PRO_ABZUG_UPFL        DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_N_PRO_ABZ_UPFL        DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ZW_BETRAG_UPFL        DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_EQF_ABZUG_UPFl        DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_BETRAG_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_ABZUG_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_KORR_FREI         DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_KORR_UPFL         DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ANTEIL_FREI           DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_ANTEIL_UPFL           DECIMAL(05, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0228                           *
      *****************************************************************
 
       01  VIVB0228-GESAMT.
        05 NI-VIVB0228-TAB.
           10 NI-VIVB0228-ELE      PIC S9(04)  BINARY OCCURS  31.
        05 NI-VIVB0228             REDEFINES   NI-VIVB0228-TAB.
           10 NI-FPRDD-ST-NR              PIC S9(04)       BINARY.
           10 NI-FPRDD-STICHTAG           PIC S9(04)       BINARY.
           10 NI-FPRDD-ANSPR-MONAT        PIC S9(04)       BINARY.
           10 NI-FPRDD-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-FPRDD-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-FPRDD-MS-NR              PIC S9(04)       BINARY.
           10 NI-FPRDD-12M-UMS-FREI       PIC S9(04)       BINARY.
           10 NI-FPRDD-12M-EQF            PIC S9(04)       BINARY.
           10 NI-FPRDD-03M-UMS-FREI       PIC S9(04)       BINARY.
           10 NI-FPRDD-PRO-ABZUG-FR       PIC S9(04)       BINARY.
           10 NI-FPRDD-N-PRO-ABZ-FR       PIC S9(04)       BINARY.
           10 NI-FPRDD-ZW-BETRAG          PIC S9(04)       BINARY.
           10 NI-FPRDD-EQF-ABZUG-FR       PIC S9(04)       BINARY.
           10 NI-FPRDD-BETRAG-FREI        PIC S9(04)       BINARY.
           10 NI-FPRDD-FAKTOR             PIC S9(04)       BINARY.
           10 NI-FPRDD-ANZ-MONATE         PIC S9(04)       BINARY.
           10 NI-FPRDD-UMS-BED-ERF        PIC S9(04)       BINARY.
           10 NI-FPRDD-S-AUSZAHLUNG       PIC S9(04)       BINARY.
           10 NI-FPRDD-S-NACH-DIR         PIC S9(04)       BINARY.
           10 NI-FPRDD-12M-UMS-UPFL       PIC S9(04)       BINARY.
           10 NI-FPRDD-03M-UMS-UPFL       PIC S9(04)       BINARY.
           10 NI-FPRDD-PRO-ABZUG-UP       PIC S9(04)       BINARY.
           10 NI-FPRDD-N-PRO-ABZ-UP       PIC S9(04)       BINARY.
           10 NI-FPRDD-ZW-BETRAG-UP       PIC S9(04)       BINARY.
           10 NI-FPRDD-EQF-ABZUG-UP       PIC S9(04)       BINARY.
           10 NI-FPRDD-BETRAG-UPFL        PIC S9(04)       BINARY.
           10 NI-FPRDD-ABZUG-NEG          PIC S9(04)       BINARY.
           10 NI-FPRDD-03M-KORR-FRE       PIC S9(04)       BINARY.
           10 NI-FPRDD-03M-KORR-UPF       PIC S9(04)       BINARY.
           10 NI-FPRDD-ANTEIL-FREI        PIC S9(04)       BINARY.
           10 NI-FPRDD-ANTEIL-UPFL        PIC S9(04)       BINARY.
 
        05 DCL-VIVB0228.
           10 FPRDD-ST-NR                 PIC S9(09)       BINARY.
           10 FPRDD-STICHTAG              PIC  X(10).
           10 FPRDD-ANSPR-MONAT           PIC  X(10).
           10 FPRDD-ANSPR-VOM             PIC  X(10).
           10 FPRDD-LAST-UPDATE           PIC  X(26).
           10 FPRDD-MS-NR                 PIC S9(04)       BINARY.
           10 FPRDD-12M-UMS-FREI          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-12M-EQF               PIC S9(03)V9(02) COMP-3.
           10 FPRDD-03M-UMS-FREI          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-PRO-ABZUG-FR          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-N-PRO-ABZ-FR          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-ZW-BETRAG             PIC S9(09)V9(02) COMP-3.
           10 FPRDD-EQF-ABZUG-FR          PIC S9(09)V9(02) COMP-3.
           10 FPRDD-BETRAG-FREI           PIC S9(09)V9(02) COMP-3.
           10 FPRDD-FAKTOR                PIC S9(03)V9(02) COMP-3.
           10 FPRDD-ANZ-MONATE            PIC S9(04)       BINARY.
           10 FPRDD-UMS-BED-ERF           PIC  X(01).
           10 FPRDD-S-AUSZAHLUNG          PIC  X(01).
           10 FPRDD-S-NACH-DIR            PIC  X(01).
           10 FPRDD-12M-UMS-UPFL          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-03M-UMS-UPFL          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-PRO-ABZUG-UP          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-N-PRO-ABZ-UP          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-ZW-BETRAG-UP          PIC S9(09)V9(02) COMP-3.
           10 FPRDD-EQF-ABZUG-UP          PIC S9(09)V9(02) COMP-3.
           10 FPRDD-BETRAG-UPFL           PIC S9(09)V9(02) COMP-3.
           10 FPRDD-ABZUG-NEG             PIC S9(09)V9(01) COMP-3.
           10 FPRDD-03M-KORR-FRE          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-03M-KORR-UPF          PIC S9(09)V9(01) COMP-3.
           10 FPRDD-ANTEIL-FREI           PIC S9(03)V9(02) COMP-3.
           10 FPRDD-ANTEIL-UPFL           PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

