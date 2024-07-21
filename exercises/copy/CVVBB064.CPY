      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB064.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:44:38   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 24 2016 14:29:20   t13ex30
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
      * DCLGEN TABLE(VIVBB064) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB064)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBB064 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , SLM_DAT_VON                 DATE NOT NULL
CCC004*    , KNR_SLM_TYP                 SMALLINT NOT NULL
CCC004*    , SLM_TURNUS                  INTEGER NOT NULL
CCC004*    , SLM_HERKUNFT                CHAR(01) NOT NULL
CCC004*    , SLM_VERARB_TYP              CHAR(01) NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , SLM_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , KTY_SLM_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , SLM_AUSZAHLUNG              DECIMAL(11, 2) NOT NULL
CCC004*    , SLM_UST_ANTEIL              DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB064                           *
      *****************************************************************
 
       01  VIVBB064-GESAMT.
        05 NI-VIVBB064-TAB.
           10 NI-VIVBB064-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVBB064             REDEFINES   NI-VIVBB064-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-SLM-DAT-VON              PIC S9(04)       BINARY.
           10 NI-KNR-SLM-TYP              PIC S9(04)       BINARY.
           10 NI-SLM-TURNUS               PIC S9(04)       BINARY.
           10 NI-SLM-HERKUNFT             PIC S9(04)       BINARY.
           10 NI-SLM-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-SLM-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-KTY-SLM-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-SLM-AUSZAHLUNG           PIC S9(04)       BINARY.
           10 NI-SLM-UST-ANTEIL           PIC S9(04)       BINARY.
 
        05 DCL-VIVBB064.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 SLM-DAT-VON                 PIC  X(10).
           10 KNR-SLM-TYP                 PIC S9(04)       BINARY.
           10 SLM-TURNUS                  PIC S9(09)       BINARY.
           10 SLM-HERKUNFT                PIC  X(01).
           10 SLM-VERARB-TYP              PIC  X(01).
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 SLM-LAST-UPDATE             PIC  X(26).
           10 KTY-SLM-TYP                 PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 SLM-AUSZAHLUNG              PIC S9(09)V9(02) COMP-3.
           10 SLM-UST-ANTEIL              PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

