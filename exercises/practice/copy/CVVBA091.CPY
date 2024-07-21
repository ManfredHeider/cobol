      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA091.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA091.CPv  $
      *
      *   Rev 2.1   Mar 28 2006 17:04:44   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.0   Mar 28 2006 09:52:30   t13ex43
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
      * DCLGEN TABLE(VIVBA091) MAPA                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA091)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVBA091 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      MAP_ANSPR_VOM               DATE NOT NULL,
CCC004*      MAP_STUFE                   CHAR(10) NOT NULL,
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,
CCC004*      MAP_LAST_UPDATE             TIMESTAMP NOT NULL,
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,
CCC004*      MAP_ANSPR_BETRAG            DECIMAL(11, 2) NOT NULL,
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL,
CCC004*      MAPA_UNTST                  INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA091
      ******************************************************************
       01  VIVBA091-GESAMT.
        05  NI-VIVBA091-TAB.
           10 NI-VIVBA091-ELE      PIC S9(04) BINARY OCCURS  12 TIMES.
        05  NI-VIVBA091            REDEFINES   NI-VIVBA091-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-MAP-ANSPR-VOM            PIC S9(04)       BINARY.
           10 NI-MAP-STUFE                PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-MAP-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-MAP-ANSPR-BETRAG         PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-MAPA-UNTST               PIC S9(04)       BINARY.

        05  DCL-VIVBA091.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 MAP-ANSPR-VOM               PIC X(10).
           10 MAP-STUFE                   PIC X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 MAP-LAST-UPDATE             PIC X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 MAP-ANSPR-BETRAG            PIC S9(09)V9(02) USAGE COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 MAPA-UNTST                  PIC S9(09)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
