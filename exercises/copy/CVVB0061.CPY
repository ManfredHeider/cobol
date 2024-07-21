      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0061.CPv  $
      *
      *   Rev 2.1   Aug 17 2005 09:13:42   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Jul 22 2005 10:45:04   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVB0061                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBVB0061 (CLPARTNER)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    08.07.2005                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!30.05.05!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(TBTP0005)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0061))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0061 TABLE

CCC004*    (
CCC004*      CLPAR_BETR_NR               INTEGER         NOT NULL
CCC004*     ,KTY_CLUB_TYP                SMALLINT        NOT NULL
CCC004*     ,KNR_CLUB_TYP                SMALLINT        NOT NULL
CCC004*     ,CLPAR_UNTST_NR              INTEGER         NOT NULL
CCC004*     ,UNTST_MS_NR                 SMALLINT
CCC004*     ,CLPAR_UNTST_EH              DECIMAL (10, 1)
CCC004*     ,CLPAR_DAT_VON               DATE            NOT NULL
CCC004*    )
CCC003*       END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0061                           *
      ******************************************************************
       01  VIVB0061-GESAMT.
           05  NI-VIVB0061-TAB.
              10 NI-VIVB0061-ELE  PIC S9(04) BINARY OCCURS 7 TIMES.
           05  NI-VIVB0061        REDEFINES  NI-VIVB0061-TAB.
              10 NI-CLPAR-BETR-NR           PIC S9(04)       BINARY.
              10 NI-KTY-CLUB-TYP            PIC S9(04)       BINARY.
              10 NI-KNR-CLUB-TYP            PIC S9(04)       BINARY.
              10 NI-CLPAR-UNTST-NR          PIC S9(04)       BINARY.
              10 NI-UNTST-MS-NR             PIC S9(04)       BINARY.
              10 NI-CLPAR-UNTST-EH          PIC S9(04)       BINARY.
              10 NI-CLPAR-DAT-VON           PIC S9(04)       BINARY.
           05  DCL-VIVB0061.
              10 CLPAR-BETR-NR              PIC S9(9)    COMP.
              10 KTY-CLUB-TYP               PIC S9(4)    COMP.
              10 KNR-CLUB-TYP               PIC S9(4)    COMP.
              10 CLPAR-UNTST-NR             PIC S9(9)    COMP.
              10 UNTST-MS-NR                PIC S9(4)    COMP.
              10 CLPAR-UNTST-EH             PIC S9(9)V9  COMP-3.
              10 CLPAR-DAT-VON              PIC X(10)      .
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
