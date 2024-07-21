      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0107.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0107.CPv  $
      *
      *   Rev 2.2   03 Sep 2007 17:29:28   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung -> 
      *PMRW0017
      *
      *   Rev 2.1   03 Sep 2007 16:55:56   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:58   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CVVB0107                                     *
      *    --------------------------------------------------------  *
      *    DCLGEN FÜR VIVB0107 (SWNS)                                *
      *    --------------------------------------------------------  *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    28.08.2007                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!28.08.07! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0107                          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0107 TABLE
CCC004*    (
CCC004*      ST_NR                  INTEGER     NOT NULL
CCC004*     ,KNR_AKZEPT_STAT        SMALLINT    NOT NULL
CCC004*     ,SWNS_DAT_BIS           DATE        NOT NULL
CCC004*     ,KTY_AKZEPT_STAT        SMALLINT    NOT NULL
CCC004*     ,SWNS_DAT_VON           TIMESTAMP   NOT NULL
CCC004*     ,SWNS_LAST_UPDATE       TIMESTAMP   NOT NULL
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0107
      ******************************************************************
       01  VIVB0107-GESAMT.
        05  NI-VIVB0107-TAB.
           10 NI-VIVB0107-ELE      PIC S9(04) BINARY OCCURS  6 TIMES.

        05  NI-VIVB0107            REDEFINES   NI-VIVB0107-TAB.
           10 NI-ST-NR                  PIC S9(04)       BINARY.
           10 NI-KNR-AKZEPT-STAT        PIC S9(04)       BINARY.
           10 NI-SWNS-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-KTY-AKZEPT-STAT        PIC S9(04)       BINARY.
           10 NI-SWNS-DAT-VON           PIC S9(04)       BINARY.
           10 NI-SWNS-LAST-UPDATE       PIC S9(04)       BINARY.

        05  DCL-VIVB0107.
           10 ST-NR                PIC S9(9)   BINARY.
           10 KNR-AKZEPT-STAT      PIC S9(4)   BINARY.
           10 SWNS-DAT-BIS         PIC X(10).
           10 KTY-AKZEPT-STAT      PIC S9(4)   BINARY.
           10 SWNS-DAT-VON         PIC X(26).
           10 SWNS-LAST-UPDATE     PIC X(26).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6
      ******************************************************************
