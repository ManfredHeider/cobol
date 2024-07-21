      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0121.CPv  $
      *
      *   Rev 2.1   Nov 21 2011 09:57:20   t13ae13
      *AAS00984: Beförderungsbedingungen 2007 auswertbar
      *
      *   Rev 2.0   Nov 08 2011 11:48:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00984!09.11.11!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0121)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0121))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0121 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MS_NR                          SMALLINT NOT NULL,
CCC004*      BEFINFO_DAT_VON                DATE NOT NULL,
CCC004*      BEFINFO_LAST_UPD               TIMESTAMP NOT NULL,
CCC004*      KTY_BEFBED_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEFBED_TYP                 SMALLINT NOT NULL,
CCC004*      BEFINFO_E_PR_ALT               CHAR(1) NOT NULL,
CCC004*      BEFINFO_S_PR_ALT               CHAR(1) NOT NULL,
CCC004*      BEFINFO_E_PR_07                CHAR(1) NOT NULL,
CCC004*      BEFINFO_S_PR_07                CHAR(1) NOT NULL,
CCC004*      BEFINFO_E_PR_11                CHAR(1) NOT NULL,
CCC004*      BEFINFO_S_PR_11                CHAR(1) NOT NULL

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0121                           *
      ******************************************************************
       01  VIVB0121-GESAMT.
        05  NI-VIVB0121-TAB.
           10  NI-VIVB0121-ELE     PIC S9(04) COMP   OCCURS 12  TIMES.
        05  NI-VIVB0121            REDEFINES  NI-VIVB0121-TAB.
           10 NI-MA-ST-NR          PIC S9(04) COMP.
           10 NI-MS-NR             PIC S9(04) COMP.
           10 NI-BEFINFO-DAT-VON   PIC S9(04) COMP.
           10 NI-BEFINFO-LAST-UPD  PIC S9(04) COMP.
           10 NI-KTY-BEFBED-TYP    PIC S9(04) COMP.
           10 NI-KNR-BEFBED-TYP    PIC S9(04) COMP.
           10 NI-BEFINFO-E-PR-ALT  PIC S9(04) COMP.
           10 NI-BEFINFO-S-PR-ALT  PIC S9(04) COMP.
           10 NI-BEFINFO-E-PR-07   PIC S9(04) COMP.
           10 NI-BEFINFO-S-PR-07   PIC S9(04) COMP.
           10 NI-BEFINFO-E-PR-11   PIC S9(04) COMP.
           10 NI-BEFINFO-S-PR-11   PIC S9(04) COMP.

        05  DCL-VIVB0121.
           10 MA-ST-NR             PIC S9(09) USAGE COMP.
           10 MS-NR                PIC S9(04) USAGE COMP.
           10 BEFINFO-DAT-VON      PIC X(10).
           10 BEFINFO-LAST-UPD     PIC X(26).
           10 KTY-BEFBED-TYP       PIC S9(04) USAGE COMP.
           10 KNR-BEFBED-TYP       PIC S9(04) USAGE COMP.
           10 BEFINFO-E-PR-ALT     PIC X(01).
           10 BEFINFO-S-PR-ALT     PIC X(01).
           10 BEFINFO-E-PR-07      PIC X(01).
           10 BEFINFO-S-PR-07      PIC X(01).
           10 BEFINFO-E-PR-11      PIC X(01).
           10 BEFINFO-S-PR-11      PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
