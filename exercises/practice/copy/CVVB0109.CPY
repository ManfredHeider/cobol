      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0109.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0109.CPv  $
      *
      *   Rev 2.2   03 Sep 2007 17:56:40   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung -> 
      *PMRW0017
      *
      *   Rev 2.1   03 Sep 2007 16:55:48   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:50   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CVVB0109                                     *
      *    --------------------------------------------------------  *
      *    DCLGEN FÜR VIVB0109 (SWNA)                                *
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
      * COBOL DECLARATION FOR TABLE VIVB0109                          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0109 TABLE
CCC004*    (
CCC004*      ST_NR                  INTEGER          NOT NULL
CCC004*     ,SWNA_DAT_BIS           DATE             NOT NULL
CCC004*     ,SWNA_DAT_VON           TIMESTAMP        NOT NULL
CCC004*     ,SWNA_LAST_UPDATE       TIMESTAMP        NOT NULL
CCC004*     ,SWNA_PRO_SATZ          DECIMAL(5, 2)
CCC004*     ,DECKEL_BETRAG          DECIMAL(11, 2)
CCC004*     ,PAUSCHAL_BETRAG        DECIMAL(11, 2)
CCC004*     ,MINIMAL_BETRAG         DECIMAL(11, 2)
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0109
      ******************************************************************
       01  VIVB0109-GESAMT.
        05  NI-VIVB0109-TAB.
           10 NI-VIVB0109-ELE      PIC S9(04) BINARY OCCURS  8 TIMES.

        05  NI-VIVB0109            REDEFINES   NI-VIVB0109-TAB.
           10 NI-ST-NR                  PIC S9(04)       BINARY.
           10 NI-SWNA-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-SWNA-DAT-VON           PIC S9(04)       BINARY.
           10 NI-SWNA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-SWNA-PRO-SATZ          PIC S9(04)       BINARY.
           10 NI-DECKEL-BETRAG          PIC S9(04)       BINARY.
           10 NI-PAUSCHAL-BETRAG        PIC S9(04)       BINARY.
           10 NI-MINIMAL-BETRAG         PIC S9(04)       BINARY.



        05  DCL-VIVB0109.
           10 ST-NR              PIC S9(9)     BINARY    VALUE ZERO.
           10 SWNA-DAT-BIS       PIC X(10)               VALUE SPACE.
           10 SWNA-DAT-VON       PIC X(26)               VALUE SPACE.
           10 SWNA-LAST-UPDATE   PIC X(26)               VALUE SPACE.
           10 SWNA-PRO-SATZ      PIC S9(03)V99 COMP-3    VALUE ZERO.
           10 DECKEL-BETRAG      PIC S9(09)V99 COMP-3    VALUE ZERO.
           10 PAUSCHAL-BETRAG    PIC S9(09)V99 COMP-3    VALUE ZERO.
           10 MINIMAL-BETRAG     PIC S9(09)V99 COMP-3    VALUE ZERO.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8
      ******************************************************************
