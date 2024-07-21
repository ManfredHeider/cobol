      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0109.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0109.CPv  $
      *
      *   Rev 2.1   03 Sep 2007 16:55:30   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:26   t13ae35
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: WVVB0109                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0109 (SWNA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    28.08.2007                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!28.08.07! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0109                           *
      ******************************************************************
        05  WB-NI-VIVB0109-TAB.
           10 WB-NI-VIVB0109-ELE   PIC S9(04)  BINARY OCCURS  8 TIMES.
        05  WB-NI-VIVB0109         REDEFINES   WB-NI-VIVB0109-TAB.
           10 WB-NI-ST-NR                  PIC S9(04)       BINARY.
           10 WB-NI-SWNA-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-SWNA-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-SWNA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-SWNA-PRO-SATZ          PIC S9(04)       BINARY.
           10 WB-NI-DECKEL-BETRAG          PIC S9(04)       BINARY.
           10 WB-NI-PAUSCHAL-BETRAG        PIC S9(04)       BINARY.
           10 WB-NI-MINIMAL-BETRAG         PIC S9(04)       BINARY.

        05  WB-VIVB0109.
           10 WB-ST-NR              PIC S9(9)     BINARY    VALUE ZERO.
           10 WB-SWNA-DAT-BIS       PIC X(10)               VALUE SPACE.
           10 WB-SWNA-DAT-VON       PIC X(26)               VALUE SPACE.
           10 WB-SWNA-LAST-UPDATE   PIC X(26)               VALUE SPACE.
           10 WB-SWNA-PRO-SATZ      PIC S9(03)V99 COMP-3    VALUE ZERO.
           10 WB-DECKEL-BETRAG      PIC S9(09)V99 COMP-3    VALUE ZERO.
           10 WB-PAUSCHAL-BETRAG    PIC S9(09)V99 COMP-3    VALUE ZERO.
           10 WB-MINIMAL-BETRAG     PIC S9(09)V99 COMP-3    VALUE ZERO.


      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
