      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA115.CPv  $
      *
      *   Rev 2.1   Oct 15 2012 09:43:20   t13ae13
      *ASL00462: Versorgungswerk 2012 - VB32
      *
      *   Rev 2.0   Sep 26 2012 08:47:20   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    26.09.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00462!26.09.12!2.1 ! NEU                       ! Walb      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA115) VSWVERS                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA115)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVBA115 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      VERS_NR                     DECIMAL(15, 0) NOT NULL,
CCC004*      STATUS_KZ                   CHAR(02) NOT NULL,
CCC004*      VSWV_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      KTY_VERS_KAT                SMALLINT,
CCC004*      KNR_VERS_KAT                SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA115
      ******************************************************************
       01  VIVBA115-GESAMT.
        05  NI-VIVBA115-TAB.
           10 NI-VIVBA115-ELE      PIC S9(04) BINARY OCCURS  6 TIMES.
        05  NI-VIVBA115            REDEFINES   NI-VIVBA115-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-VERS-NR                  PIC S9(04)       BINARY.
           10 NI-STATUS-KZ                PIC S9(04)       BINARY.
           10 NI-VSWV-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-VERS-KAT             PIC S9(04)       BINARY.
           10 NI-KNR-VERS-KAT             PIC S9(04)       BINARY.

        05  DCL-VIVBA115.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 VERS-NR                     PIC S9(15)       COMP-3.
           10 STATUS-KZ                   PIC  X(02).
           10 VSWV-LAST-UPDATE            PIC  X(26).
           10 KTY-VERS-KAT                PIC S9(04)       BINARY.
           10 KNR-VERS-KAT                PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
