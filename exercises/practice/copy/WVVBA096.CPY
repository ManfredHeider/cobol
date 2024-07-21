      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA096.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA096.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 13:03:48   t13ex13
      *AVB01198: Bef.bed.2011 in EBB aufnehmen
      *
      *   Rev 2.0   Jan 05 2012 16:53:06   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA096                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA096 (EBB)                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: RALF GUTFLEISCH                                  *
      *    DATUM:    05.01.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01198!05.01.12!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA096                          *
      *****************************************************************
        05  WB-NI-VIVBA096-TAB.
           10 WB-NI-VIVBA096-ELE      PIC S9(04) BINARY OCCURS 30 TIMES.
        05  WB-NI-VIVBA096            REDEFINES  WB-NI-VIVBA096-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(4)    BINARY.
           10 WB-NI-KNR-TYP-BEFBED    PIC S9(4)    BINARY.
           10 WB-NI-EBB-DAT           PIC S9(4)    BINARY.
           10 WB-NI-KTY-TYP-BEFBED    PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-EIG        PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-EIG-KZ     PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-EIG       PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-EIG-KZ    PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-GRU        PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-GRU-KZ     PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-GRU       PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-GRU-KZ    PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-GES        PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-GES-KZ     PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-GES       PIC S9(4)    BINARY.
           10 WB-NI-EBB-EQF-GES-KZ    PIC S9(4)    BINARY.
           10 WB-NI-EBB-PROZ-KLAUS    PIC S9(4)    BINARY.
           10 WB-NI-EBB-EIGEN-MAX     PIC S9(4)    BINARY.
           10 WB-NI-EBB-KORR-EH-GRU   PIC S9(4)    BINARY.
           10 WB-NI-EBB-KORR-EH-GES   PIC S9(4)    BINARY.
           10 WB-NI-EBB-KORR-KZ       PIC S9(4)    BINARY.
           10 WB-NI-EBB-PART          PIC S9(4)    BINARY.
           10 WB-NI-EBB-AP-KZ         PIC S9(4)    BINARY.
           10 WB-NI-EBB-KZ            PIC S9(4)    BINARY.
           10 WB-NI-EBB-LAST-UPDATE   PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-UV-GES     PIC S9(4)    BINARY.
           10 WB-NI-EBB-EH-UV-GES-KZ  PIC S9(4)    BINARY.
           10 WB-NI-EBB-PART-ERS      PIC S9(4)    BINARY.
           10 WB-NI-EBB-PART-RGS      PIC S9(4)    BINARY.
           10 WB-NI-EBB-AP-RGS-KZ     PIC S9(4)    BINARY.
        05  WB-VIVBA096.
           10 WB-MA-ST-NR             PIC S9(9) USAGE COMP.
           10 WB-KNR-TYP-BEFBED       PIC S9(4) USAGE COMP.
           10 WB-EBB-DAT              PIC X(10).
           10 WB-KTY-TYP-BEFBED       PIC S9(4) USAGE COMP.
           10 WB-EBB-EH-EIG           PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-EH-EIG-KZ        PIC X(1).
           10 WB-EBB-EQF-EIG          PIC S9(3)V9(2) USAGE COMP-3.
           10 WB-EBB-EQF-EIG-KZ       PIC X(1).
           10 WB-EBB-EH-GRU           PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-EH-GRU-KZ        PIC X(1).
           10 WB-EBB-EQF-GRU          PIC S9(3)V9(2) USAGE COMP-3.
           10 WB-EBB-EQF-GRU-KZ       PIC X(1).
           10 WB-EBB-EH-GES           PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-EH-GES-KZ        PIC X(1).
           10 WB-EBB-EQF-GES          PIC S9(3)V9(2) USAGE COMP-3.
           10 WB-EBB-EQF-GES-KZ       PIC X(1).
           10 WB-EBB-PROZ-KLAUS       PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-EIGEN-MAX        PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-KORR-EH-GRU      PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-KORR-EH-GES      PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-KORR-KZ          PIC X(1).
           10 WB-EBB-PART             PIC S9(4) USAGE COMP.
           10 WB-EBB-AP-KZ            PIC X(1).
           10 WB-EBB-KZ               PIC X(1).
           10 WB-EBB-LAST-UPDATE      PIC X(26).
           10 WB-EBB-EH-UV-GES        PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-EBB-EH-UV-GES-KZ     PIC X(1).
           10 WB-EBB-PART-ERS         PIC S9(4) USAGE COMP.
           10 WB-EBB-PART-RGS         PIC S9(4) USAGE COMP.
           10 WB-EBB-AP-RGS-KZ        PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
