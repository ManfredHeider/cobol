      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0505.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0505.CPv  $
      *
      *   Rev 2.1   May 04 2016 13:38:12   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0505
      *
      *   Rev 2.0   Apr 21 2016 15:54:16   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0505                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0505 (USTSATZ)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    21.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!21.04.16!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0505                           *
      *****************************************************************
 
        05 WB-NI-VIVB0505-TAB.
           10 WB-NI-VIVB0505-ELE   PIC S9(04)  BINARY OCCURS   9.
        05 WB-NI-VIVB0505          REDEFINES   WB-NI-VIVB0505-TAB.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-UST-SATZ              PIC S9(04)       BINARY.
           10 WB-NI-UST-SCHLUESSEL        PIC S9(04)       BINARY.
           10 WB-NI-UST-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-UST-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KTY-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-UST-LAST-UPDATE       PIC S9(04)       BINARY.
 
        05 WB-VIVB0505.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 WB-UST-SATZ                 PIC S9(03)V9(03) COMP-3.
           10 WB-UST-SCHLUESSEL           PIC S9(04)       BINARY.
           10 WB-UST-DAT-VON              PIC  X(10).
           10 WB-UST-DAT-BIS              PIC  X(10).
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 WB-UST-LAST-UPDATE          PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************

