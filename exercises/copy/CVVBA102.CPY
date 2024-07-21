      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA102.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:45:24   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 25 2016 10:24:58   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    25.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!25.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA102) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA102)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA102 TABLE
CCC004*    ( BUCH_DAT_VON                DATE NOT NULL
CCC004*    , MA_ST_NR                    INTEGER NOT NULL
CCC004*    , KTY_BUCH_TYP                SMALLINT NOT NULL
CCC004*    , KNR_BUCH_TYP                SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , BETRAG                      DECIMAL(11, 2) NOT NULL
CCC004*    , BUCH_TEXT                   CHAR(40) NOT NULL
CCC004*    , KONTO_TYP                   CHAR(01) NOT NULL
CCC004*    , BELEGART                    CHAR(03) NOT NULL
CCC004*    , BELEG_NR                    CHAR(07) NOT NULL
CCC004*    , TURNUS                      CHAR(06) NOT NULL
CCC004*    , BELEG_DAT                   CHAR(08) NOT NULL
CCC004*    , BUCH_DAT                    CHAR(08) NOT NULL
CCC004*    , KOSTEN_STELLE               CHAR(08) NOT NULL
CCC004*    , GEGEN_KONTO                 CHAR(12) NOT NULL
CCC004*    , SHKZ                        CHAR(01) NOT NULL
CCC004*    , BUCH_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , STEUER_JAHR                 SMALLINT NOT NULL
CCC004*    , BEZUGS_JAHR                 SMALLINT NOT NULL
CCC004*    , BEZUG_LFD                   INTEGER NOT NULL
CCC004*    , ABR_NR                      CHAR(15) NOT NULL
CCC004*    , KTY_VERARB_TYP              SMALLINT NOT NULL
CCC004*    , KNR_VERARB_TYP              SMALLINT NOT NULL
CCC004*    , ZIEL_STATUS                 CHAR(01) NOT NULL
CCC004*    , STEUER_SCHLUESSEL           SMALLINT NOT NULL
CCC004*    , UST_ANTEIL_U                DECIMAL(11, 2) NOT NULL
CCC004*    , GEGEN_KONTO_INFO            CHAR(12) NOT NULL
CCC004*    , BUCH_TEXT2                  CHAR(40) NOT NULL
CCC004*    , KTY_LEISTUNGSART            SMALLINT NOT NULL
CCC004*    , KNR_LEISTUNGSART            SMALLINT NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA102                           *
      *****************************************************************
 
       01  VIVBA102-GESAMT.
        05 NI-VIVBA102-TAB.
           10 NI-VIVBA102-ELE      PIC S9(04)  BINARY OCCURS  31.
        05 NI-VIVBA102             REDEFINES   NI-VIVBA102-TAB.
           10 NI-BUCH-DAT-VON             PIC S9(04)       BINARY.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-KTY-BUCH-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-BUCH-TYP             PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-BETRAG                   PIC S9(04)       BINARY.
           10 NI-BUCH-TEXT                PIC S9(04)       BINARY.
           10 NI-KONTO-TYP                PIC S9(04)       BINARY.
           10 NI-BELEGART                 PIC S9(04)       BINARY.
           10 NI-BELEG-NR                 PIC S9(04)       BINARY.
           10 NI-TURNUS                   PIC S9(04)       BINARY.
           10 NI-BELEG-DAT                PIC S9(04)       BINARY.
           10 NI-BUCH-DAT                 PIC S9(04)       BINARY.
           10 NI-KOSTEN-STELLE            PIC S9(04)       BINARY.
           10 NI-GEGEN-KONTO              PIC S9(04)       BINARY.
           10 NI-SHKZ                     PIC S9(04)       BINARY.
           10 NI-BUCH-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-STEUER-JAHR              PIC S9(04)       BINARY.
           10 NI-BEZUGS-JAHR              PIC S9(04)       BINARY.
           10 NI-BEZUG-LFD                PIC S9(04)       BINARY.
           10 NI-ABR-NR                   PIC S9(04)       BINARY.
           10 NI-KTY-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-ZIEL-STATUS              PIC S9(04)       BINARY.
           10 NI-STEUER-SCHLUESSEL        PIC S9(04)       BINARY.
           10 NI-UST-ANTEIL-U             PIC S9(04)       BINARY.
           10 NI-GEGEN-KONTO-INFO         PIC S9(04)       BINARY.
           10 NI-BUCH-TEXT2               PIC S9(04)       BINARY.
           10 NI-KTY-LEISTUNGSART         PIC S9(04)       BINARY.
           10 NI-KNR-LEISTUNGSART         PIC S9(04)       BINARY.
 
        05 DCL-VIVBA102.
           10 BUCH-DAT-VON                PIC  X(10).
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KTY-BUCH-TYP                PIC S9(04)       BINARY.
           10 KNR-BUCH-TYP                PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 BETRAG                      PIC S9(09)V9(02) COMP-3.
           10 BUCH-TEXT                   PIC  X(40).
           10 KONTO-TYP                   PIC  X(01).
           10 BELEGART                    PIC  X(03).
           10 BELEG-NR                    PIC  X(07).
           10 TURNUS                      PIC  X(06).
           10 BELEG-DAT                   PIC  X(08).
           10 BUCH-DAT                    PIC  X(08).
           10 KOSTEN-STELLE               PIC  X(08).
           10 GEGEN-KONTO                 PIC  X(12).
           10 SHKZ                        PIC  X(01).
           10 BUCH-LAST-UPDATE            PIC  X(26).
           10 STEUER-JAHR                 PIC S9(04)       BINARY.
           10 BEZUGS-JAHR                 PIC S9(04)       BINARY.
           10 BEZUG-LFD                   PIC S9(09)       BINARY.
           10 ABR-NR                      PIC  X(15).
           10 KTY-VERARB-TYP              PIC S9(04)       BINARY.
           10 KNR-VERARB-TYP              PIC S9(04)       BINARY.
           10 ZIEL-STATUS                 PIC  X(01).
           10 STEUER-SCHLUESSEL           PIC S9(04)       BINARY.
           10 UST-ANTEIL-U                PIC S9(09)V9(02) COMP-3.
           10 GEGEN-KONTO-INFO            PIC  X(12).
           10 BUCH-TEXT2                  PIC  X(40).
           10 KTY-LEISTUNGSART            PIC S9(04)       BINARY.
           10 KNR-LEISTUNGSART            PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

