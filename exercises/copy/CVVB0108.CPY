      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0108.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CVVB0108.CPv  $
      *
      *   Rev 2.3   03 Sep 2007 17:56:34   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung -> 
      *PMRW0017
      *
      *   Rev 2.2   03 Sep 2007 17:29:20   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung -> 
      *PMRW0017
      *
      *   Rev 2.1   03 Sep 2007 16:55:42   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:42   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CVVB0108                                     *
      *    --------------------------------------------------------  *
      *    DCLGEN FÜR VIVB0108 (SWNB)                                *
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
      * COBOL DECLARATION FOR TABLE VIVB0108                          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0108 TABLE
CCC004*    (
CCC004*      ST_NR                  INTEGER          NOT NULL
CCC004*     ,SWNB_ERM_VOM           DATE             NOT NULL
CCC004*     ,KNR_ERM_ART            SMALLINT         NOT NULL
CCC004*     ,KTY_ERM_ART            SMALLINT         NOT NULL
CCC004*     ,SWNB_MS_NR             SMALLINT         NOT NULL
CCC004*     ,SWNB_LAST_UPDATE       TIMESTAMP        NOT NULL
CCC004*     ,KTY_WAEHRUNG           SMALLINT         NOT NULL
CCC004*     ,KNR_WAEHRUNG           SMALLINT         NOT NULL
CCC004*     ,SWNB_BETRAG            DECIMAL(11, 2)    NOT NULL
CCC004*     ,KTY_BERECH_STAT        SMALLINT         NOT NULL
CCC004*     ,KNR_BERECH_STAT        SMALLINT         NOT NULL
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0108
      ******************************************************************
       01  VIVB0108-GESAMT.
        05  NI-VIVB0108-TAB.
           10 NI-VIVB0108-ELE      PIC S9(04) BINARY OCCURS  11 TIMES.

        05  NI-VIVB0108            REDEFINES   NI-VIVB0108-TAB.
           10 NI-ST-NR                  PIC S9(04)       BINARY.
           10 NI-SWNB-ERM-VOM           PIC S9(04)       BINARY.
           10 NI-KNR-ERM-ART            PIC S9(04)       BINARY.
           10 NI-KTY-ERM-ART            PIC S9(04)       BINARY.
           10 NI-SWNB-MS-NR             PIC S9(04)       BINARY.
           10 NI-SWNB-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG           PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG           PIC S9(04)       BINARY.
           10 NI-SWNB-BETRAG            PIC S9(04)       BINARY.
           10 NI-KTY-BERECH-STAT        PIC S9(04)       BINARY.
           10 NI-KNR-BERECH-STAT        PIC S9(04)       BINARY.


        05  DCL-VIVB0108.
           10 ST-NR                PIC S9(9)    BINARY   VALUE ZERO.
           10 SWNB-ERM-VOM         PIC X(10)             VALUE SPACE.
           10 KNR-ERM-ART          PIC S9(4)    BINARY   VALUE ZERO.
           10 KTY-ERM-ART          PIC S9(4)    BINARY   VALUE ZERO.
           10 SWNB-MS-NR           PIC S9(4)    BINARY   VALUE ZERO.
           10 SWNB-LAST-UPDATE     PIC X(26)             VALUE SPACE.
           10 KTY-WAEHRUNG         PIC S9(4)    BINARY   VALUE ZERO.
           10 KNR-WAEHRUNG         PIC S9(4)    BINARY   VALUE ZERO.
           10 SWNB-BETRAG          PIC S9(9)V99 COMP-3   VALUE ZERO.
           10 KTY-BERECH-STAT      PIC S9(4)    BINARY   VALUE ZERO.
           10 KNR-BERECH-STAT      PIC S9(4)    BINARY   VALUE ZERO.


      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11
      ******************************************************************

