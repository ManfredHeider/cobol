      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0214.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CVVB0214.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:42:52   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:51:42   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:38:22   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:29:38   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:14:54   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:05:18   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 13 2011 13:14:38   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    13.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00420!13.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0214) PABOA                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0214)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0214 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , PABOA_UNTST                 INTEGER NOT NULL
CCC004*    , PABOA_ANSPR_VOM             DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , PABA_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , PABA_ANSP_BETRAG            DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0214                           *
      *****************************************************************

       01  VIVB0214-GESAMT.
        05 NI-VIVB0214-TAB.
           10 NI-VIVB0214-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVB0214             REDEFINES   NI-VIVB0214-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-PABOA-UNTST              PIC S9(04)       BINARY.
           10 NI-PABOA-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-PABA-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-PABA-ANSP-BETRAG         PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05 DCL-VIVB0214.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 PABOA-UNTST                 PIC S9(09)       BINARY.
           10 PABOA-ANSPR-VOM             PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 PABA-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 PABA-ANSP-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
