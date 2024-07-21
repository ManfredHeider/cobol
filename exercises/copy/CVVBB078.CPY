      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB078.CPv  $
      *
      *   Rev 2.1   Mar 28 2006 17:04:56   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.0   Mar 28 2006 09:52:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    28.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00226!28.03.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB078) KAND                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB078)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVBB078 TABLE
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
CCC004*      KAND_INFO                      CHAR(20)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB078
      ******************************************************************
       01  VIVBB078-GESAMT.
        05  NI-VIVBB078-TAB.
           10 NI-VIVBB078-ELE      PIC S9(04) BINARY OCCURS 12  TIMES.
        05  NI-VIVBB078            REDEFINES   NI-VIVBB078-TAB.
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
        05 DCL-VIVBB078.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
