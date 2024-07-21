      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.05.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBC078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBC078.CPv  $
      *
      *   Rev 2.1   13 Jun 2007 09:57:24   t13ae27
      *ASL00269: MAP für RGS 2.Teil
      *
      *   Rev 2.0   29 May 2007 14:34:52   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    29.05.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00269!29.05.07!2.1 ! NEU                       ! P.Duerr   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBC078) KAND                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC078)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVBC078 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KTY_WB_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_WB_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_WERTUNGS_TYP               SMALLINT NOT NULL,
CCC004*      KNR_WERTUNGS_TYP               SMALLINT NOT NULL,
CCC004*      KAND_WERTUNGSSTUFE             SMALLINT NOT NULL,
CCC004*      KAND_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KAND_TAETIG_BIS                DATE NOT NULL,
CCC004*      KAND_DRUCK_KZ                  SMALLINT NOT NULL,
CCC004*      KAND_NEU_AB                    DATE,
CCC004*      KAND_RUECKERM                  CHAR,
CCC004*      KAND_INFO                      CHAR(20),
CCC004*      KAND_START_DATUM               DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC078
      ******************************************************************
       01  VIVBC078-GESAMT.
        05  NI-VIVBC078-TAB.
           10 NI-VIVBC078-ELE      PIC S9(04) BINARY OCCURS 13 TIMES.
        05  NI-VIVBC078            REDEFINES   NI-VIVBC078-TAB.
           10 NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP             PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP             PIC S9(04)       BINARY.
           10 NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.
           10 NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.
           10 NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.
           10 NI-KAND-NEU-AB            PIC S9(04)       BINARY.
           10 NI-KAND-RUECKERM          PIC S9(04)       BINARY.
           10 NI-KAND-INFO              PIC S9(04)       BINARY.
           10 NI-KAND-START-DATUM       PIC S9(04)       BINARY.
        05 DCL-VIVBC078.
           10 MA-ST-NR                  PIC S9(9)        BINARY.
           10 KTY-WB-TYP                PIC S9(4)        BINARY.
           10 KNR-WB-TYP                PIC S9(4)        BINARY.
           10 KTY-WERTUNGS-TYP          PIC S9(4)        BINARY.
           10 KNR-WERTUNGS-TYP          PIC S9(4)        BINARY.
           10 KAND-WERTUNGSSTUFE        PIC S9(4)        BINARY.
           10 KAND-LAST-UPDATE          PIC X(26).
           10 KAND-TAETIG-BIS           PIC X(10).
           10 KAND-DRUCK-KZ             PIC S9(4)        BINARY.
           10 KAND-NEU-AB               PIC X(010).
           10 KAND-RUECKERM             PIC X(001).
           10 KAND-INFO                 PIC X(020).
           10 KAND-START-DATUM          PIC X(010).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
