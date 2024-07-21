      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0400.CPv  $
      *
      *   Rev 2.1   Jun 06 2016 14:09:26   t13ae14
      *AVB01400: Projekt Lucy2 - Füllen Steuerwechsel-Tabelle STAB
      *
      *   Rev 2.0   May 12 2016 09:59:56   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: OBER                                             *
      *    DATUM:    12.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!12.05.16!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0400) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0400)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0400 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , STAB_BUCH_DAT               DATE NOT NULL
CCC004*    , STAB_BEZUGS_JAHR            SMALLINT NOT NULL
CCC004*    , STAB_STEUER_JAHR            SMALLINT NOT NULL
CCC004*    , STAB_DAT_HERKUNFT           CHAR(01) NOT NULL
CCC004*    , STAB_LFD                    INTEGER NOT NULL
CCC004*    , STAB_BEZUG_LFD              INTEGER NOT NULL
CCC004*    , STAB_ABR_NR                 CHAR(15)
CCC004*    , KTY_VERARB_TYP              SMALLINT NOT NULL
CCC004*    , KNR_VERARB_TYP              SMALLINT NOT NULL
CCC004*    , STAB_SHKZ                   CHAR(01) NOT NULL
CCC004*    , STAB_BUCH_TEXT              CHAR(40) NOT NULL
CCC004*    , STAB_BUCH_TEXT2             CHAR(52) NOT NULL
CCC004*    , KTY_USTAT_TYP               SMALLINT NOT NULL
CCC004*    , KNR_USTAT_TYP               SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , STAB_KONTO_TYP              CHAR(01) NOT NULL
CCC004*    , STAB_BELEGART               CHAR(03) NOT NULL
CCC004*    , STAB_BELEG_NR               CHAR(07) NOT NULL
CCC004*    , STAB_TURNUS                 CHAR(06) NOT NULL
CCC004*    , STAB_BELEG_DAT              CHAR(08) NOT NULL
CCC004*    , STAB_KOSTEN_STELLE          CHAR(08) NOT NULL
CCC004*    , STAB_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , STAB_KONTO_KU               CHAR(12) NOT NULL
CCC004*    , STAB_BETRAG_KU              DECIMAL(11, 2) NOT NULL
CCC004*    , STAB_KONTO_U                CHAR(12) NOT NULL
CCC004*    , STAB_BETRAG_U               DECIMAL(11, 2) NOT NULL
CCC004*    , STAB_UST_ANTEIL_U           DECIMAL(11, 2) NOT NULL
CCC004*    , STAB_STS                    SMALLINT NOT NULL
CCC004*    , STAB_UST_BERECHNET          CHAR(01) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0400                           *
      *****************************************************************
 
       01  VIVB0400-GESAMT.
        05 NI-VIVB0400-TAB.
           10 NI-VIVB0400-ELE      PIC S9(04)  BINARY OCCURS  31.
        05 NI-VIVB0400             REDEFINES   NI-VIVB0400-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-STAB-BUCH-DAT            PIC S9(04)       BINARY.
           10 NI-STAB-BEZUGS-JAHR         PIC S9(04)       BINARY.
           10 NI-STAB-STEUER-JAHR         PIC S9(04)       BINARY.
           10 NI-STAB-DAT-HERKUNFT        PIC S9(04)       BINARY.
           10 NI-STAB-LFD                 PIC S9(04)       BINARY.
           10 NI-STAB-BEZUG-LFD           PIC S9(04)       BINARY.
           10 NI-STAB-ABR-NR              PIC S9(04)       BINARY.
           10 NI-KTY-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-STAB-SHKZ                PIC S9(04)       BINARY.
           10 NI-STAB-BUCH-TEXT           PIC S9(04)       BINARY.
           10 NI-STAB-BUCH-TEXT2          PIC S9(04)       BINARY.
           10 NI-KTY-USTAT-TYP            PIC S9(04)       BINARY.
           10 NI-KNR-USTAT-TYP            PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-STAB-KONTO-TYP           PIC S9(04)       BINARY.
           10 NI-STAB-BELEGART            PIC S9(04)       BINARY.
           10 NI-STAB-BELEG-NR            PIC S9(04)       BINARY.
           10 NI-STAB-TURNUS              PIC S9(04)       BINARY.
           10 NI-STAB-BELEG-DAT           PIC S9(04)       BINARY.
           10 NI-STAB-KOSTEN-STELLE       PIC S9(04)       BINARY.
           10 NI-STAB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-STAB-KONTO-KU            PIC S9(04)       BINARY.
           10 NI-STAB-BETRAG-KU           PIC S9(04)       BINARY.
           10 NI-STAB-KONTO-U             PIC S9(04)       BINARY.
           10 NI-STAB-BETRAG-U            PIC S9(04)       BINARY.
           10 NI-STAB-UST-ANTEIL-U        PIC S9(04)       BINARY.
           10 NI-STAB-STS                 PIC S9(04)       BINARY.
           10 NI-STAB-UST-BERECHNET       PIC S9(04)       BINARY.
 
        05 DCL-VIVB0400.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 STAB-BUCH-DAT               PIC  X(10).
           10 STAB-BEZUGS-JAHR            PIC S9(04)       BINARY.
           10 STAB-STEUER-JAHR            PIC S9(04)       BINARY.
           10 STAB-DAT-HERKUNFT           PIC  X(01).
           10 STAB-LFD                    PIC S9(09)       BINARY.
           10 STAB-BEZUG-LFD              PIC S9(09)       BINARY.
           10 STAB-ABR-NR                 PIC  X(15).
           10 KTY-VERARB-TYP              PIC S9(04)       BINARY.
           10 KNR-VERARB-TYP              PIC S9(04)       BINARY.
           10 STAB-SHKZ                   PIC  X(01).
           10 STAB-BUCH-TEXT              PIC  X(40).
           10 STAB-BUCH-TEXT2             PIC  X(52).
           10 KTY-USTAT-TYP               PIC S9(04)       BINARY.
           10 KNR-USTAT-TYP               PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 STAB-KONTO-TYP              PIC  X(01).
           10 STAB-BELEGART               PIC  X(03).
           10 STAB-BELEG-NR               PIC  X(07).
           10 STAB-TURNUS                 PIC  X(06).
           10 STAB-BELEG-DAT              PIC  X(08).
           10 STAB-KOSTEN-STELLE          PIC  X(08).
           10 STAB-LAST-UPDATE            PIC  X(26).
           10 STAB-KONTO-KU               PIC  X(12).
           10 STAB-BETRAG-KU              PIC S9(09)V9(02) COMP-3.
           10 STAB-KONTO-U                PIC  X(12).
           10 STAB-BETRAG-U               PIC S9(09)V9(02) COMP-3.
           10 STAB-UST-ANTEIL-U           PIC S9(09)V9(02) COMP-3.
           10 STAB-STS                    PIC S9(04)       BINARY.
           10 STAB-UST-BERECHNET          PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

