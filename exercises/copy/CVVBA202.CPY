      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA202.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:37:36   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA088, ZVVBA202
      *
      *   Rev 2.0   Apr 21 2016 13:44:16   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    22.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!22.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA202) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA202)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA202 TABLE
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
CCC004*    , BOZ_12M_UMS_UPFL            DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_03M_UMS_UPFL            DECIMAL(10, 1) NOT NULL
CCC004*    , EH_PRO_ABZUG_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , EH_N_PRO_ABZ_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_ZW_BETRAG_UPFL          DECIMAL(11, 2) NOT NULL
CCC004*    , EQF_ABZUG_UPFL              DECIMAL(11, 2) NOT NULL
CCC004*    , BOZ_BETRAG_UPFL             DECIMAL(11, 2) NOT NULL
CCC004*    , EH_ABZUG_NEGATIV            DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_03M_KORRIGIERT          DECIMAL(10, 1) NOT NULL
CCC004*    , BOZ_03M_KORR_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , EH_ANTEIL                   DECIMAL(05, 2) NOT NULL
CCC004*    , EH_ANTEIL_UPFL              DECIMAL(05, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA202                           *
      *****************************************************************
 
       01  VIVBA202-GESAMT.
        05 NI-VIVBA202-TAB.
           10 NI-VIVBA202-ELE      PIC S9(04)  BINARY OCCURS  31.
        05 NI-VIVBA202             REDEFINES   NI-VIVBA202-TAB.
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
           10 NI-BOZ-12M-UMS-UPFL         PIC S9(04)       BINARY.
           10 NI-BOZ-03M-UMS-UPFL         PIC S9(04)       BINARY.
           10 NI-EH-PRO-ABZUG-UPFL        PIC S9(04)       BINARY.
           10 NI-EH-N-PRO-ABZ-UPFL        PIC S9(04)       BINARY.
           10 NI-BOZ-ZW-BETRAG-UPFL       PIC S9(04)       BINARY.
           10 NI-EQF-ABZUG-UPFL           PIC S9(04)       BINARY.
           10 NI-BOZ-BETRAG-UPFL          PIC S9(04)       BINARY.
           10 NI-EH-ABZUG-NEGATIV         PIC S9(04)       BINARY.
           10 NI-BOZ-03M-KORRIGIERT       PIC S9(04)       BINARY.
           10 NI-BOZ-03M-KORR-UPFL        PIC S9(04)       BINARY.
           10 NI-EH-ANTEIL                PIC S9(04)       BINARY.
           10 NI-EH-ANTEIL-UPFL           PIC S9(04)       BINARY.
 
        05 DCL-VIVBA202.
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
           10 BOZ-12M-UMS-UPFL            PIC S9(09)V9(01) COMP-3.
           10 BOZ-03M-UMS-UPFL            PIC S9(09)V9(01) COMP-3.
           10 EH-PRO-ABZUG-UPFL           PIC S9(09)V9(01) COMP-3.
           10 EH-N-PRO-ABZ-UPFL           PIC S9(09)V9(01) COMP-3.
           10 BOZ-ZW-BETRAG-UPFL          PIC S9(09)V9(02) COMP-3.
           10 EQF-ABZUG-UPFL              PIC S9(09)V9(02) COMP-3.
           10 BOZ-BETRAG-UPFL             PIC S9(09)V9(02) COMP-3.
           10 EH-ABZUG-NEGATIV            PIC S9(09)V9(01) COMP-3.
           10 BOZ-03M-KORRIGIERT          PIC S9(09)V9(01) COMP-3.
           10 BOZ-03M-KORR-UPFL           PIC S9(09)V9(01) COMP-3.
           10 EH-ANTEIL                   PIC S9(03)V9(02) COMP-3.
           10 EH-ANTEIL-UPFL              PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

