      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0117.CPv  $
      *
      *   Rev 2.1   Apr 30 2010 12:11:50   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - Tabellen
      *
      *   Rev 2.0   Mar 06 2009 09:37:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!22.01.10!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0117)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0117))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0117 TABLE
CCC004*    ( EDW_DL_ALT                     INTEGER NOT NULL,
CCC004*      EDW_UNT_ST_NR                  INTEGER NOT NULL,
CCC004*      EDW_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EDW_DL_NEU                     INTEGER NOT NULL,
CCC004*      EDW_DAT_VON                    DATE NOT NULL,
CCC004*      EDW_DAT_BIS                    DATE,
CCC004*      KTY_EDW_STATUS                 SMALLINT NOT NULL,
CCC004*      KNR_EDW_STATUS                 SMALLINT NOT NULL,
CCC004*      EDW_DIR_NR_ALT                 SMALLINT NOT NULL,
CCC004*      EDW_DIR_NR_NEU                 SMALLINT NOT NULL

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0117                           *
      ******************************************************************
       01  VIVB0117-GESAMT.
        05  NI-VIVB0117-TAB.
           10  NI-VIVB0117-ELE     PIC S9(04) COMP   OCCURS 10  TIMES.
        05  NI-VIVB0117            REDEFINES  NI-VIVB0117-TAB.
           10 NI-EDW-DL-ALT        PIC S9(04) COMP.
           10 NI-EDW-UNT-ST-NR     PIC S9(04) COMP.
           10 NI-EDW-LAST-UPDATE   PIC S9(04) COMP.
           10 NI-EDW-DL-NEU        PIC S9(04) COMP.
           10 NI-EDW-DAT-VON       PIC S9(04) COMP.
           10 NI-EDW-DAT-BIS       PIC S9(04) COMP.
           10 NI-KTY-EDW-STATUS    PIC S9(04) COMP.
           10 NI-KNR-EDW-STATUS    PIC S9(04) COMP.
           10 NI-EDW-DIR-NR-ALT    PIC S9(04) COMP.
           10 NI-EDW-DIR-NR-NEU    PIC S9(04) COMP.

        05  DCL-VIVB0117.
           10 EDW-DL-ALT           PIC S9(09) USAGE COMP.
           10 EDW-UNT-ST-NR        PIC S9(09) USAGE COMP.
           10 EDW-LAST-UPDATE      PIC X(26).
           10 EDW-DL-NEU           PIC S9(09) USAGE COMP.
           10 EDW-DAT-VON          PIC X(10).
           10 EDW-DAT-BIS          PIC X(10).
           10 KTY-EDW-STATUS       PIC S9(04) USAGE COMP.
           10 KNR-EDW-STATUS       PIC S9(04) USAGE COMP.
           10 EDW-DIR-NR-ALT       PIC S9(04) USAGE COMP.
           10 EDW-DIR-NR-NEU       PIC S9(04) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
