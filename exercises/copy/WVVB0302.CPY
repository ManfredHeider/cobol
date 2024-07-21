      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0302.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0302.CPv  $
      *
      *   Rev 2.2   Mar 26 2009 14:13:48   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.1   Mar 26 2009 09:20:56   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 25 2009 08:43:38   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0302                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0302 (DETAILNP)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    25.03.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!25.03.09!2.1 ! NEU                       ! KLEEMEIER *
260309* AAS00812!26.03.09!2.2 ! KORRERKTUR FELDNAMEN      ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0302                          *
      *****************************************************************
        05  WB-NI-VIVB0302-TAB.
           10 WB-NI-VIVB0302-ELE   PIC S9(04)  BINARY OCCURS 12 TIMES.
        05  WB-NI-VIVB0302         REDEFINES   WB-NI-VIVB0302-TAB.
           10 WB-NI-DETAILNP-BETR         PIC S9(04)       BINARY.
           10 WB-NI-DETAILNP-MONAT        PIC S9(04)       BINARY.
           10 WB-NI-DETAILNP-UNTST        PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP      PIC S9(04)       BINARY.
260309**** 10 WB-NI-DETAILNP-LAST-UPD     PIC S9(04)       BINARY.
260309     10 WB-NI-DETAILNP-LAST-UP      PIC S9(04)       BINARY.
           10 WB-NI-DETAILNP-EH           PIC S9(04)       BINARY.
           10 WB-NI-DETAILNP-EBENE        PIC S9(04)       BINARY.
           10 WB-NI-DETAILNP-BIS          PIC S9(04)       BINARY.
260309**** 10 WB-NI-PARTEH-BETR-TYP       PIC S9(04)       BINARY.
260309     10 WB-NI-DETAILNP-BTR-TYP      PIC S9(04)       BINARY.

        05  WB-VIVB0302.
           10 WB-DETAILNP-BETR            PIC S9(09)       BINARY.
           10 WB-DETAILNP-MONAT           PIC  X(10).
           10 WB-DETAILNP-UNTST           PIC S9(09)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KTY-WERTUNGS-TYP         PIC S9(04)       BINARY.
260309**** 10 WB-DETAILNP-LAST-UPD        PIC  X(26).
260309     10 WB-DETAILNP-LAST-UP         PIC  X(26).
           10 WB-DETAILNP-EH              PIC S9(09)V9(01) COMP-3.
           10 WB-DETAILNP-EBENE           PIC  X(01).
           10 WB-DETAILNP-BIS             PIC  X(10).
260309**** 10 WB-PARTEH-BETR-TYP          PIC  X(01).
260309     10 WB-DETAILNP-BTR-TYP         PIC  X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
