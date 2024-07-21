      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0119.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0119.CPv  $
      *
      *   Rev 2.1   22 Mar 2007 11:47:00   t13ex43
      *AKI00651: Kundensuche in Akt. für OKI anpassen
      *
      *   Rev 2.0   19 Mar 2007 16:00:02   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    19.03.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00651!19.03.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      * DCLGEN TABLE(TCOK0119) PILOT_VBNR                             *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTOK0119)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TCOK0119 TABLE
CCC004*    ( PILOT_VBNR                  INTEGER NOT NULL,
CCC004*      LETZTEAENDERUNG             TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TCOK0119
      ******************************************************************
       01  TCOK0119-GESAMT.
        05  NI-TCOK0119-TAB.
           10 NI-TCOK0119-ELE      PIC S9(04) BINARY OCCURS  2 TIMES.
        05  NI-TCOK0119            REDEFINES   NI-TCOK0119-TAB.
           10 NI-PILOT-VBNR               PIC S9(04)       BINARY.
           10 NI-LETZTEAENDERUNG          PIC S9(04)       BINARY.

        05  DCL-TCOK0119.
           10 PILOT-VBNR                  PIC S9(09)       BINARY.
           10 LETZTEAENDERUNG             PIC  X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
