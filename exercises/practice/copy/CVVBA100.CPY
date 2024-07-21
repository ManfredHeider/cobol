      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA100.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:45:28   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 25 2016 14:24:10   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    25.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!25.05.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA100) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA100)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA100 TABLE
CCC004*    ( KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , FIBUZ_KONTO                 CHAR(12) NOT NULL
CCC004*    , FIBUZ_DAT_VON               TIMESTAMP NOT NULL
CCC004*    , FIBUZ_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , FIBUZ_DAT_BIS               DATE NOT NULL
CCC004*    , FIBUZ_INFO                  CHAR(30) NOT NULL
CCC004*    , KTY_LEISTUNGSTYP            SMALLINT NOT NULL
CCC004*    , KNR_LEISTUNGSTYP            SMALLINT NOT NULL
CCC004*    , KTY_DATUM                   SMALLINT NOT NULL
CCC004*    , KNR_DATUM                   SMALLINT NOT NULL
CCC004*    , KTY_LEISTUNGSART            SMALLINT NOT NULL
CCC004*    , KNR_LEISTUNGSART            SMALLINT NOT NULL
CCC004*    , FIBUZ_STAB_RELEVAN          CHAR(01) NOT NULL
CCC004*    , FIBUZ_UST_STATUS            CHAR(01) NOT NULL
CCC004*    , FIBUZ_ZUGEH_KONTO           CHAR(12) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA100                           *
      *****************************************************************
 
       01  VIVBA100-GESAMT.
        05 NI-VIVBA100-TAB.
           10 NI-VIVBA100-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVBA100             REDEFINES   NI-VIVBA100-TAB.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-FIBUZ-KONTO              PIC S9(04)       BINARY.
           10 NI-FIBUZ-DAT-VON            PIC S9(04)       BINARY.
           10 NI-FIBUZ-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-FIBUZ-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-FIBUZ-INFO               PIC S9(04)       BINARY.
           10 NI-KTY-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 NI-KNR-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 NI-KTY-DATUM                PIC S9(04)       BINARY.
           10 NI-KNR-DATUM                PIC S9(04)       BINARY.
           10 NI-KTY-LEISTUNGSART         PIC S9(04)       BINARY.
           10 NI-KNR-LEISTUNGSART         PIC S9(04)       BINARY.
           10 NI-FIBUZ-STAB-RELEVAN       PIC S9(04)       BINARY.
           10 NI-FIBUZ-UST-STATUS         PIC S9(04)       BINARY.
           10 NI-FIBUZ-ZUGEH-KONTO        PIC S9(04)       BINARY.
 
        05 DCL-VIVBA100.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 FIBUZ-KONTO                 PIC  X(12).
           10 FIBUZ-DAT-VON               PIC  X(26).
           10 FIBUZ-LAST-UPDATE           PIC  X(26).
           10 FIBUZ-DAT-BIS               PIC  X(10).
           10 FIBUZ-INFO                  PIC  X(30).
           10 KTY-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 KNR-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 KTY-DATUM                   PIC S9(04)       BINARY.
           10 KNR-DATUM                   PIC S9(04)       BINARY.
           10 KTY-LEISTUNGSART            PIC S9(04)       BINARY.
           10 KNR-LEISTUNGSART            PIC S9(04)       BINARY.
           10 FIBUZ-STAB-RELEVAN          PIC  X(01).
           10 FIBUZ-UST-STATUS            PIC  X(01).
           10 FIBUZ-ZUGEH-KONTO           PIC  X(12).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

