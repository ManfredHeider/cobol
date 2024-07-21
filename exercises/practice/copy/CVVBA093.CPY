      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA093.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:35:34   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA093 u. ZVVBB094
      *
      *   Rev 2.0   Apr 19 2016 09:22:46   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    19.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!19.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA093) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA093)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA093 TABLE
CCC004*    ( LZUA_ST_NR                  INTEGER NOT NULL
CCC004*    , KTY_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , LZUA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , LZUA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , LZUA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , LZUA_ANSP_BET_UPFL          DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA093                           *
      *****************************************************************
 
       01  VIVBA093-GESAMT.
        05 NI-VIVBA093-TAB.
           10 NI-VIVBA093-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 NI-VIVBA093             REDEFINES   NI-VIVBA093-TAB.
           10 NI-LZUA-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-LZUA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-LZUA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-LZUA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-LZUA-ANSP-BET-UPFL       PIC S9(04)       BINARY.
 
        05 DCL-VIVBA093.
           10 LZUA-ST-NR                  PIC S9(09)       BINARY.
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 LZUA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 LZUA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 LZUA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 LZUA-ANSP-BET-UPFL          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************

