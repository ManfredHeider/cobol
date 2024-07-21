      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0124.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0124.CPv  $
      *
      *   Rev 2.1   Jan 24 2013 12:14:06   t13ex57
      *AVB01257: Mailadresse für VM
      *
      *   Rev 2.0   Nov 21 2012 14:45:18   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0124                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0124 (MAINFO  )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    21.11.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01257!21.11.12!2.1 ! NEU                       ! BOLL      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0124                           *
      *****************************************************************

        05 WB-NI-VIVB0124-TAB.
           10 WB-NI-VIVB0124-ELE   PIC S9(04)  BINARY OCCURS   7.
        05 WB-NI-VIVB0124          REDEFINES   WB-NI-VIVB0124-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KTY-MAINFO-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-MAINFO-TYP        PIC S9(04)       BINARY.
           10 WB-NI-MAINFO-DAT-VON        PIC S9(04)       BINARY.
           10 WB-NI-MAINFO-LAST-UPD       PIC S9(04)       BINARY.
           10 WB-NI-MAINFO-DAT-BIS        PIC S9(04)       BINARY.
           10 WB-NI-MAINFO-INFO           PIC S9(04)       BINARY.

        05 WB-VIVB0124.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-KTY-MAINFO-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-MAINFO-TYP           PIC S9(04)       BINARY.
           10 WB-MAINFO-DAT-VON           PIC  X(26).
           10 WB-MAINFO-LAST-UPD          PIC  X(26).
           10 WB-MAINFO-DAT-BIS           PIC  X(10).
           10 WB-MAINFO-INFO              PIC  X(80).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************
