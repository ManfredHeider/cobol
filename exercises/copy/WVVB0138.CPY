      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0138.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0138.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:35:20   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Sep 09 2020 13:23:48   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0138                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0138 (PEHDAI  )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T13AE30                                          *
      *    DATUM:    09.09.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-989 !09.09.20!2.1 ! NEU                       ! T13AE30   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0138                          *
      *****************************************************************
 
        05 WB-NI-VIVB0138-TAB.
           10 WB-NI-VIVB0138-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0138          REDEFINES   WB-NI-VIVB0138-TAB.
           10 WB-NI-PEHDAI-VS-ZEITRAUM    PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-BETR           PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-UNTST          PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-DAT-VON        PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-DAT-BIS        PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-ERSTELL-DAT    PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-EH-DAISY       PIC S9(04)       BINARY.
           10 WB-NI-PEHDAI-UNTST-WERT     PIC S9(04)       BINARY.
 
        05 WB-VIVB0138.
           10 WB-PEHDAI-VS-ZEITRAUM       PIC S9(04)       BINARY.
           10 WB-PEHDAI-BETR              PIC S9(09)       BINARY.
           10 WB-PEHDAI-UNTST             PIC S9(09)       BINARY.
           10 WB-PEHDAI-DAT-VON           PIC  X(10).
           10 WB-PEHDAI-DAT-BIS           PIC  X(10).
           10 WB-PEHDAI-ERSTELL-DAT       PIC  X(10).
           10 WB-PEHDAI-EH-DAISY          PIC S9(09)V9(01) COMP-3.
           10 WB-PEHDAI-UNTST-WERT        PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

