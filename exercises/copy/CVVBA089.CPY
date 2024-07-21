      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA089.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA089.CPv  $
      *
      *   Rev 2.1   May 04 2016 14:37:34   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA089
      *
      *   Rev 2.0   May 03 2016 14:31:18   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    03.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!03.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA089) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA089)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA089 TABLE
CCC004*    ( BOZB_ST_NR                  INTEGER NOT NULL
CCC004*    , BOZB_STICHTAG               DATE NOT NULL
CCC004*    , BOZB_ANSPR_VOM              DATE NOT NULL
CCC004*    , BOZB_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , BOZB_MS_NR                  SMALLINT NOT NULL
CCC004*    , BOZB_12MON_UMSATZ           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_12MON_EQF              DECIMAL(05, 2) NOT NULL
CCC004*    , BOZB_03MON_UMSATZ           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_EQF_ABZUG              DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_FAKTOR                 DECIMAL(05, 2)
CCC004*    , BOZB_ANZ_MONATE             SMALLINT
CCC004*    , BOZB_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_EQF_ABZ_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , BOZB_EH_ABZ_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_03M_KORRIG             DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_03M_KORR_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , BOZB_EH_ANTEIL              DECIMAL(05, 2) NOT NULL
CCC004*    , BOZB_EH_ANT_UPFL            DECIMAL(05, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA089                           *
      *****************************************************************
 
       01  VIVBA089-GESAMT.
        05 NI-VIVBA089-TAB.
           10 NI-VIVBA089-ELE      PIC S9(04)  BINARY OCCURS  19.
        05 NI-VIVBA089             REDEFINES   NI-VIVBA089-TAB.
           10 NI-BOZB-ST-NR               PIC S9(04)       BINARY.
           10 NI-BOZB-STICHTAG            PIC S9(04)       BINARY.
           10 NI-BOZB-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-BOZB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-BOZB-MS-NR               PIC S9(04)       BINARY.
           10 NI-BOZB-12MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-BOZB-12MON-EQF           PIC S9(04)       BINARY.
           10 NI-BOZB-03MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-BOZB-EQF-ABZUG           PIC S9(04)       BINARY.
           10 NI-BOZB-FAKTOR              PIC S9(04)       BINARY.
           10 NI-BOZB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 NI-BOZB-12M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-BOZB-03M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-BOZB-EQF-ABZ-UPFL        PIC S9(04)       BINARY.
           10 NI-BOZB-EH-ABZ-NEG          PIC S9(04)       BINARY.
           10 NI-BOZB-03M-KORRIG          PIC S9(04)       BINARY.
           10 NI-BOZB-03M-KORR-UPFL       PIC S9(04)       BINARY.
           10 NI-BOZB-EH-ANTEIL           PIC S9(04)       BINARY.
           10 NI-BOZB-EH-ANT-UPFL         PIC S9(04)       BINARY.
 
        05 DCL-VIVBA089.
           10 BOZB-ST-NR                  PIC S9(09)       BINARY.
           10 BOZB-STICHTAG               PIC  X(10).
           10 BOZB-ANSPR-VOM              PIC  X(10).
           10 BOZB-LAST-UPDATE            PIC  X(26).
           10 BOZB-MS-NR                  PIC S9(04)       BINARY.
           10 BOZB-12MON-UMSATZ           PIC S9(09)V9(01) COMP-3.
           10 BOZB-12MON-EQF              PIC S9(03)V9(02) COMP-3.
           10 BOZB-03MON-UMSATZ           PIC S9(09)V9(01) COMP-3.
           10 BOZB-EQF-ABZUG              PIC S9(09)V9(01) COMP-3.
           10 BOZB-FAKTOR                 PIC S9(03)V9(02) COMP-3.
           10 BOZB-ANZ-MONATE             PIC S9(04)       BINARY.
           10 BOZB-12M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 BOZB-03M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 BOZB-EQF-ABZ-UPFL           PIC S9(09)V9(02) COMP-3.
           10 BOZB-EH-ABZ-NEG             PIC S9(09)V9(01) COMP-3.
           10 BOZB-03M-KORRIG             PIC S9(09)V9(01) COMP-3.
           10 BOZB-03M-KORR-UPFL          PIC S9(09)V9(01) COMP-3.
           10 BOZB-EH-ANTEIL              PIC S9(03)V9(02) COMP-3.
           10 BOZB-EH-ANT-UPFL            PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

