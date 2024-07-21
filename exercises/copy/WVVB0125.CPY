      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0125.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0125.CPv  $
      *
      *   Rev 2.2   Apr 28 2015 12:57:50   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.1   Mar 13 2015 10:00:32   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   Mar 13 2015 08:58:42   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0125                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0125 (USTAT   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    27.04.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!27.04.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0125                           *
      *****************************************************************
 
        05 WB-NI-VIVB0125-TAB.
           10 WB-NI-VIVB0125-ELE   PIC S9(04)  BINARY OCCURS  10.
        05 WB-NI-VIVB0125          REDEFINES   WB-NI-VIVB0125-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-USTAT-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-KNR-USTAT-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KTY-USTAT-TYP         PIC S9(04)       BINARY.
           10 WB-NI-USTAT-LAST-UPD        PIC S9(04)       BINARY.
           10 WB-NI-USTAT-DAT-BIS         PIC S9(04)       BINARY.
           10 WB-NI-USTAT-AEND-DURCH      PIC S9(04)       BINARY.
           10 WB-NI-ZUGANG                PIC S9(04)       BINARY.
           10 WB-NI-VON                   PIC S9(04)       BINARY.
           10 WB-NI-BIS                   PIC S9(04)       BINARY.
 
        05 WB-VIVB0125.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-USTAT-DAT-VON            PIC  X(26).
           10 WB-KNR-USTAT-TYP            PIC S9(04)       BINARY.
           10 WB-KTY-USTAT-TYP            PIC S9(04)       BINARY.
           10 WB-USTAT-LAST-UPD           PIC  X(26).
           10 WB-USTAT-DAT-BIS            PIC  X(10).
           10 WB-USTAT-AEND-DURCH         PIC  X(08).
           10 WB-ZUGANG                   PIC  X(26).
           10 WB-VON                      PIC  X(26).
           10 WB-BIS                      PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

