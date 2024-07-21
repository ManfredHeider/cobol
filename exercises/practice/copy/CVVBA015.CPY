      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA015.CPv  $
      *
      *   Rev 2.1   Apr 23 2008 11:27:06   t13ae48
      *AVB00875: Verlängerung der Adressfelder  - MAAN (JMS)
      *
      *   Rev 2.0   Mar 10 2008 13:24:34   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVBA015                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.MAESS                                          *
      *    DATUM:    10.03.2008                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00875!10.03.08!2.1 ! NEU                       ! J.MAESS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA015)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA015))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA015 TABLE
CCC004*    ( MAAN_BEZUGS_NR                 INTEGER NOT NULL,
CCC004*      KTY_MAAN_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_MAAN_TYP                   SMALLINT NOT NULL,
CCC004*      MAAN_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      MAAN_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      MAAN_STRASSE                   CHAR(40) NOT NULL,
CCC004*      MAAN_PLZ_AKTUELL               CHAR(9) NOT NULL,
CCC004*      MAAN_PLZ                       CHAR(9) NOT NULL,
CCC004*      MAAN_ORT                       CHAR(40) NOT NULL,
CCC004*      MAAN_PLZ_PF                    CHAR(9) NOT NULL,
CCC004*      MAAN_ORT_PF                    CHAR(40) NOT NULL,
CCC004*      MAAN_POSTFACH                  CHAR(6) NOT NULL,
CCC004*      MAAN_WOHNT_BEI                 CHAR(40) NOT NULL,
CCC004*      MAAN_DAT_BIS                   DATE NOT NULL,
CCC004*      MAAN_TELEFON                   CHAR(26) NOT NULL,
CCC004*      MAAN_TELEFAX                   CHAR(26) NOT NULL,
CCC004*      KTY_AUSL                       SMALLINT NOT NULL,
CCC004*      KNR_AUSL                       SMALLINT NOT NULL,
CCC004*      KTY_GRND                       SMALLINT NOT NULL,
CCC004*      KNR_GRND                       SMALLINT NOT NULL,
CCC004*      KTY_ZUSTELLUNG                 SMALLINT NOT NULL,
CCC004*      KNR_ZUSTELLUNG                 SMALLINT NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA015                           *
      ******************************************************************
       01  VIVBA015-GESAMT.
        05  NI-VIVBA015-TAB.
           10  NI-VIVBA015-ELE     PIC S9(04)  COMP   OCCURS  24 TIMES.
        05  NI-VIVBA015            REDEFINES   NI-VIVBA015-TAB.
           10 NI-MAAN-BEZUGS-NR    PIC S9(04)  COMP.
           10 NI-KTY-MAAN-TYP      PIC S9(04)  COMP.
           10 NI-KNR-MAAN-TYP      PIC S9(04)  COMP.
           10 NI-MAAN-DAT-VON      PIC S9(04)  COMP.
           10 NI-MAAN-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-MAAN-STRASSE      PIC S9(04)  COMP.
           10 NI-MAAN-PLZ-AKTUELL  PIC S9(04)  COMP.
           10 NI-MAAN-PLZ          PIC S9(04)  COMP.
           10 NI-MAAN-ORT          PIC S9(04)  COMP.
           10 NI-MAAN-PLZ-PF       PIC S9(04)  COMP.
           10 NI-MAAN-ORT-PF       PIC S9(04)  COMP.
           10 NI-MAAN-POSTFACH     PIC S9(04)  COMP.
           10 NI-MAAN-WOHNT-BEI    PIC S9(04)  COMP.
           10 NI-MAAN-DAT-BIS      PIC S9(04)  COMP.
           10 NI-MAAN-TELEFON      PIC S9(04)  COMP.
           10 NI-MAAN-TELEFAX      PIC S9(04)  COMP.
           10 NI-KTY-AUSL          PIC S9(04)  COMP.
           10 NI-KNR-AUSL          PIC S9(04)  COMP.
           10 NI-KTY-GRND          PIC S9(04)  COMP.
           10 NI-KNR-GRND          PIC S9(04)  COMP.
           10 NI-KTY-ZUSTELLUNG    PIC S9(04)  COMP.
           10 NI-KNR-ZUSTELLUNG    PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
        05  DCL-VIVBA015.
           10 MAAN-BEZUGS-NR       PIC S9(9) USAGE COMP.
           10 KTY-MAAN-TYP         PIC S9(4) USAGE COMP.
           10 KNR-MAAN-TYP         PIC S9(4) USAGE COMP.
           10 MAAN-DAT-VON         PIC X(26).
           10 MAAN-LAST-UPDATE     PIC X(26).
           10 MAAN-STRASSE         PIC X(40).
           10 MAAN-PLZ-AKTUELL     PIC X(9).
           10 MAAN-PLZ             PIC X(9).
           10 MAAN-ORT             PIC X(40).
           10 MAAN-PLZ-PF          PIC X(9).
           10 MAAN-ORT-PF          PIC X(40).
           10 MAAN-POSTFACH        PIC X(6).
           10 MAAN-WOHNT-BEI       PIC X(40).
           10 MAAN-DAT-BIS         PIC X(10).
           10 MAAN-TELEFON         PIC X(26).
           10 MAAN-TELEFAX         PIC X(26).
           10 KTY-AUSL             PIC S9(4) USAGE COMP.
           10 KNR-AUSL             PIC S9(4) USAGE COMP.
           10 KTY-GRND             PIC S9(4) USAGE COMP.
           10 KNR-GRND             PIC S9(4) USAGE COMP.
           10 KTY-ZUSTELLUNG       PIC S9(4) USAGE COMP.
           10 KNR-ZUSTELLUNG       PIC S9(4) USAGE COMP.
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************


