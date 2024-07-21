      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.05.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBC078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBC078.CPv  $
      *
      *   Rev 2.1   13 Jun 2007 09:56:58   t13ae27
      *ASL00269: MAP für RGS 2.Teil
      *
      *   Rev 2.0   29 May 2007 14:40:40   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBC078                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBC078 (KAND)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    29.05.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00269!29.05.07!2.1 ! NEU                       ! P. Duerr  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC078                          *
      *****************************************************************
        05  WB-NI-VIVBC078-TAB.
           10 WB-NI-VIVBC078-ELE   PIC S9(04)  BINARY OCCURS 13 TIMES.
        05  WB-NI-VIVBC078         REDEFINES   WB-NI-VIVBC078-TAB.
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
           10 WB-NI-KAND-START-DATUM       PIC S9(04)       BINARY.

        05  WB-VIVBC078.
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
           10 WB-KAND-START-DATUM         PIC X(010).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
