      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0115.CPv  $
      *
      *   Rev 2.1   Sep 10 2008 11:52:28   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.0   Aug 12 2008 07:28:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    12.08.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!12.08.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0115) VSWVERS                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0115)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0115 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      VERS_NR                     DECIMAL(15, 0) NOT NULL,
CCC004*      STATUS_KZ                   CHAR(02) NOT NULL,
CCC004*      VSWV_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0115
      ******************************************************************
       01  VIVB0115-GESAMT.
        05  NI-VIVB0115-TAB.
           10 NI-VIVB0115-ELE      PIC S9(04) BINARY OCCURS  4 TIMES.
        05  NI-VIVB0115            REDEFINES   NI-VIVB0115-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-VERS-NR                  PIC S9(04)       BINARY.
           10 NI-STATUS-KZ                PIC S9(04)       BINARY.
           10 NI-VSWV-LAST-UPDATE         PIC S9(04)       BINARY.

        05  DCL-VIVB0115.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 VERS-NR                     PIC S9(15)       COMP-3.
           10 STATUS-KZ                   PIC  X(02).
           10 VSWV-LAST-UPDATE            PIC  X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
