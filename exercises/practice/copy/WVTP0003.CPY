      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  11.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVTP0003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WVTP0003.CPv  $
      *
      *   Rev 2.1   May 15 2003 14:03:04   t13ae13
      *AVB00658: Dimension 7ukunft - Neues Aufstiegssystem 
      *
      *   Rev 2.0   Mar 11 2003 15:31:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVTP0003                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    02.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00658!02.04.03!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  TBTP0003 BEN                       *
      ******************************************************************
        05  WB-NI-TBTP0003-TAB.
           10 WB-NI-TBTP0003-ELE      PIC S9(04) COMP  OCCURS 8 TIMES.
        05  WB-NI-TBTP0003            REDEFINES  WB-NI-TBTP0003-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-MONAT         PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-NEU-STF       PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-ZEITRAUM      PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-EH-GS-NT-GR   PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-EH-GS-NT-GES  PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-ANZ-HPTB-ME   PIC S9(4) USAGE COMP.
           10 WB-NI-BEN-ANZ-AB-VBA    PIC S9(4) USAGE COMP.
        05  WB-TBTP0003.
           10 WB-MA-ST-NR             PIC  S9(09)    COMP.
           10 WB-BEN-MONAT            PIC   X(10).
           10 WB-BEN-NEU-STF          PIC  S9(04)    COMP.
           10 WB-BEN-ZEITRAUM         PIC  S9(04)    COMP.
           10 WB-BEN-EH-GS-NT-GR      PIC  S9(09)V9  COMP-3.
           10 WB-BEN-EH-GS-NT-GES     PIC  S9(09)V9  COMP-3.
           10 WB-BEN-ANZ-HPTB-ME      PIC  S9(04)    COMP.
           10 WB-BEN-ANZ-AB-VBA       PIC  S9(04)    COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
