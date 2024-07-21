      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA064.CPv  $
      *
      *   Rev 2.1   Sep 08 2011 15:53:58   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.0   Aug 05 2011 16:04:06   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA064                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA064 (SLM)                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00070!08.08.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA064                           *
      *****************************************************************

        05 WB-NI-VIVBA064-TAB.
           10 WB-NI-VIVBA064-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVBA064          REDEFINES   WB-NI-VIVBA064-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-SLM-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-KNR-SLM-TYP           PIC S9(04)       BINARY.
           10 WB-NI-SLM-TURNUS            PIC S9(04)       BINARY.
           10 WB-NI-SLM-HERKUNFT          PIC S9(04)       BINARY.
           10 WB-NI-SLM-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-SLM-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-SLM-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-SLM-AUSZAHLUNG        PIC S9(04)       BINARY.

        05 WB-VIVBA064.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-SLM-DAT-VON              PIC  X(10).
           10 WB-KNR-SLM-TYP              PIC S9(04)       BINARY.
           10 WB-SLM-TURNUS               PIC S9(09)       BINARY.
           10 WB-SLM-HERKUNFT             PIC  X(01).
           10 WB-SLM-VERARB-TYP           PIC  X(01).
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-SLM-LAST-UPDATE          PIC  X(26).
           10 WB-KTY-SLM-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-SLM-AUSZAHLUNG           PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
