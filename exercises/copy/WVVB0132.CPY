      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0132.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0132.CPv  $
      *
      *   Rev 2.1   Jun 28 2019 12:37:06   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 07 2019 14:54:06   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0132                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0132 (GUTSC)                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-998 !07.06.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0132                          *
      *****************************************************************
 
        05 WB-NI-VIVB0132-TAB.
           10 WB-NI-VIVB0132-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0132          REDEFINES   WB-NI-VIVB0132-TAB.
           10 WB-NI-GUTSC-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-KTY-GUTSC-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KNR-GUTSC-TYP         PIC S9(04)       BINARY.
           10 WB-NI-GUTSC-CODE            PIC S9(04)       BINARY.
           10 WB-NI-GUTSC-GUELT-AB        PIC S9(04)       BINARY.
           10 WB-NI-GUTSC-GUELT-BIS       PIC S9(04)       BINARY.
           10 WB-NI-GUTSC-MELD-DAT        PIC S9(04)       BINARY.
           10 WB-NI-GUTSC-LAST-UPD        PIC S9(04)       BINARY.
 
        05 WB-VIVB0132.
           10 WB-GUTSC-ST-NR              PIC S9(09)       BINARY.
           10 WB-KTY-GUTSC-TYP            PIC S9(04)       BINARY.
           10 WB-KNR-GUTSC-TYP            PIC S9(04)       BINARY.
           10 WB-GUTSC-CODE               PIC  X(20).
           10 WB-GUTSC-GUELT-AB           PIC  X(10).
           10 WB-GUTSC-GUELT-BIS          PIC  X(10).
           10 WB-GUTSC-MELD-DAT           PIC  X(10).
           10 WB-GUTSC-LAST-UPD           PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
