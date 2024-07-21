      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0313.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0313.CPv  $
      *
      *   Rev 2.1   Jul 07 2011 15:49:20   t13ae30
      *AAS00964: Zustrukturierung von Partnern
      *
      *   Rev 2.0   May 11 2011 17:07:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0313                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0313 (DETAILDW)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    30.06.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00964!30.06.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0313                           *
      *****************************************************************

        05 WB-NI-VIVB0313-TAB.
           10 WB-NI-VIVB0313-ELE   PIC S9(04)  BINARY OCCURS   9.
        05 WB-NI-VIVB0313          REDEFINES   WB-NI-VIVB0313-TAB.
           10 WB-NI-DETAILDW-BETR         PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-UNTST        PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-JAHR         PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-MONAT        PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-LAST-UP      PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-EH           PIC S9(04)       BINARY.
           10 WB-NI-DETAILDW-BIS          PIC S9(04)       BINARY.

        05 WB-VIVB0313.
           10 WB-DETAILDW-BETR            PIC S9(09)       BINARY.
           10 WB-DETAILDW-UNTST           PIC S9(09)       BINARY.
           10 WB-DETAILDW-JAHR            PIC S9(04)       BINARY.
           10 WB-DETAILDW-MONAT           PIC  X(10).
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-DETAILDW-LAST-UP         PIC  X(26).
           10 WB-DETAILDW-EH              PIC S9(09)V9(01) COMP-3.
           10 WB-DETAILDW-BIS             PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************
