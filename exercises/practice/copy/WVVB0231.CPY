      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0231.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0231.CPv  $
      *
      *   Rev 2.2   19 Nov 2019 08:33:52   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.1   Aug 27 2019 07:55:20   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.0   May 24 2019 16:00:36   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0231                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0231 (VHV)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00584!19.06.19!2.1 ! NEU                       ! KLEEMEIER *
111119* ASL00584!11.11.19!2.2 ! VHV-ANZAHL HINZU          ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0231                          *
      *****************************************************************
 
        05 WB-NI-VIVB0231-TAB.
111119**** 10 WB-NI-VIVB0231-ELE   PIC S9(04)  BINARY OCCURS  14.
111119     10 WB-NI-VIVB0231-ELE   PIC S9(04)  BINARY OCCURS  15.
        05 WB-NI-VIVB0231          REDEFINES   WB-NI-VIVB0231-TAB.
           10 WB-NI-VHV-ST-NR             PIC S9(04)       BINARY.
           10 WB-NI-KNR-VERSICH-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KTY-VERSICH-TYP       PIC S9(04)       BINARY.
           10 WB-NI-VHV-BEGINN-DAT        PIC S9(04)       BINARY.
           10 WB-NI-VHV-LAST-UPD          PIC S9(04)       BINARY.
           10 WB-NI-VHV-ENDE-DAT          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-VHV-VER-BETRAG        PIC S9(04)       BINARY.
           10 WB-NI-VHV-VER-NR            PIC S9(04)       BINARY.
           10 WB-NI-VHV-ERST-BETRAG       PIC S9(04)       BINARY.
           10 WB-NI-VHV-ERST-DAT          PIC S9(04)       BINARY.
           10 WB-NI-VHV-INFO              PIC S9(04)       BINARY.
           10 WB-NI-VHV-EINZ-VT           PIC S9(04)       BINARY.
111119     10 WB-NI-VHV-ANZAHL            PIC S9(04)       BINARY.
 
        05 WB-VIVB0231.
           10 WB-VHV-ST-NR                PIC S9(09)       BINARY.
           10 WB-KNR-VERSICH-TYP          PIC S9(04)       BINARY.
           10 WB-KTY-VERSICH-TYP          PIC S9(04)       BINARY.
           10 WB-VHV-BEGINN-DAT           PIC  X(10).
           10 WB-VHV-LAST-UPD             PIC  X(26).
           10 WB-VHV-ENDE-DAT             PIC  X(10).
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-VHV-VER-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 WB-VHV-VER-NR               PIC S9(15)       COMP-3.
           10 WB-VHV-ERST-BETRAG          PIC S9(09)V9(02) COMP-3.
           10 WB-VHV-ERST-DAT             PIC  X(10).
           10 WB-VHV-INFO                 PIC  X(100).
           10 WB-VHV-EINZ-VT              PIC  X(50).
111119     10 WB-VHV-ANZAHL               PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *
      *****************************************************************

