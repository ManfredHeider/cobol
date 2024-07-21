      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA088.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA088.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:37:32   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA088, ZVVBA202
      *
      *   Rev 2.0   Apr 21 2016 13:44:10   t13ex30
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
      * DCLGEN TABLE(VIVBA088) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA088)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA088 TABLE
CCC004*    ( BOZA_ST_NR                  INTEGER NOT NULL
CCC004*    , BOZA_ANSPR_VOM              DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , BOZA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , BOZA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KNR_ANSPR_STATUS            SMALLINT
CCC004*    , BOZA_ANSP_BET_UPFL          DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA088                           *
      *****************************************************************
 
       01  VIVBA088-GESAMT.
        05 NI-VIVBA088-TAB.
           10 NI-VIVBA088-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVBA088             REDEFINES   NI-VIVBA088-TAB.
           10 NI-BOZA-ST-NR               PIC S9(04)       BINARY.
           10 NI-BOZA-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-BOZA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-BOZA-ANSPR-BETRAG        PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-BOZA-ANSP-BET-UPFL       PIC S9(04)       BINARY.
 
        05 DCL-VIVBA088.
           10 BOZA-ST-NR                  PIC S9(09)       BINARY.
           10 BOZA-ANSPR-VOM              PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 BOZA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 BOZA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 BOZA-ANSP-BET-UPFL          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************

