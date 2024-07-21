      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA036.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA036.CPv  $
      *
      *   Rev 2.1   Mar 04 2015 10:15:28   t13ae30
      *ASL00530: Ext.grd.-Zuschuss - Anz. Monate für VBI
      *
      *   Rev 2.0   Mar 03 2015 16:14:30   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA036                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA036 (ANZ     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    03.03.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!03.03.15!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA036                           *
      *****************************************************************

        05 WB-NI-VIVBA036-TAB.
           10 WB-NI-VIVBA036-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVBA036          REDEFINES   WB-NI-VIVBA036-TAB.
           10 WB-NI-ANZ-MONAT             PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-ANZ-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-ANZ-WERT              PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.

        05 WB-VIVBA036.
           10 WB-ANZ-MONAT                PIC  X(10).
           10 WB-KNR-ANZ-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KTY-ANZ-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-ANZ-LAST-UPDATE          PIC  X(26).
           10 WB-ANZ-WERT                 PIC S9(13)V9(02) COMP-3.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

