      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB078.CPv  $
      *
      *   Rev 2.1   Mar 28 2006 17:04:32   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.0   Mar 28 2006 09:52:14   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB078                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB078 (KAND)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    28.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00226!28.03.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB078                          *
      *****************************************************************
        05  WB-NI-VIVBB078-TAB.
           10 WB-NI-VIVBB078-ELE   PIC S9(04)  BINARY OCCURS 12 TIMES.
        05  WB-NI-VIVBB078         REDEFINES   WB-NI-VIVBB078-TAB.
           10 WB-NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP             PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP             PIC S9(04)       BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.
           10 WB-NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.
           10 WB-NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.
           10 WB-NI-KAND-NEU-AB            PIC S9(04)       BINARY.
           10 WB-NI-KAND-RUECKERM          PIC S9(04)       BINARY.
           10 WB-NI-KAND-INFO              PIC S9(04)       BINARY.

        05  WB-VIVBB078.
           10 WB-MA-ST-NR                 PIC S9(009)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(004)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(004)       BINARY.
           10 WB-KTY-WERTUNGS-TYP         PIC S9(004)       BINARY.
           10 WB-KNR-WERTUNGS-TYP         PIC S9(004)       BINARY.
           10 WB-KAND-WERTUNGSSTUFE       PIC S9(004)       BINARY.
           10 WB-KAND-LAST-UPDATE         PIC X(026).
           10 WB-KAND-TAETIG-BIS          PIC X(010).
           10 WB-KAND-DRUCK-KZ            PIC S9(004)       BINARY.
           10 WB-KAND-NEU-AB              PIC X(010).
           10 WB-KAND-RUECKERM            PIC X(001).
           10 WB-KAND-INFO                PIC X(020).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
