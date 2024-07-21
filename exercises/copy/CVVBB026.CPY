      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB026.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:44:44   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 24 2016 14:29:26   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    24.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!24.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB026) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB026)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBB026 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , OLZZ_MONAT                  DATE NOT NULL
CCC004*    , KNR_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , OLZZ_TURNUS                 INTEGER NOT NULL
CCC004*    , OLZZ_HERKUNFT               CHAR(01) NOT NULL
CCC004*    , OLZZ_VERARB_TYP             CHAR(01) NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , OLZZ_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , OLZZ_BETRAG                 DECIMAL(11, 2) NOT NULL
CCC004*    , OLZZ_UST_ANTEIL             DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB026                           *
      *****************************************************************
 
       01  VIVBB026-GESAMT.
        05 NI-VIVBB026-TAB.
           10 NI-VIVBB026-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVBB026             REDEFINES   NI-VIVBB026-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-OLZZ-MONAT               PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-OLZZ-TURNUS              PIC S9(04)       BINARY.
           10 NI-OLZZ-HERKUNFT            PIC S9(04)       BINARY.
           10 NI-OLZZ-VERARB-TYP          PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-OLZZ-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-OLZZ-BETRAG              PIC S9(04)       BINARY.
           10 NI-OLZZ-UST-ANTEIL          PIC S9(04)       BINARY.
 
        05 DCL-VIVBB026.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 OLZZ-MONAT                  PIC  X(10).
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 OLZZ-TURNUS                 PIC S9(09)       BINARY.
           10 OLZZ-HERKUNFT               PIC  X(01).
           10 OLZZ-VERARB-TYP             PIC  X(01).
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 OLZZ-LAST-UPDATE            PIC  X(26).
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 OLZZ-BETRAG                 PIC S9(09)V9(02) COMP-3.
           10 OLZZ-UST-ANTEIL             PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

