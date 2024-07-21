      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBA034.CPv  $
      *
      *   Rev 2.2   Jun 24 2008 12:58:16   t13ae30
      *ASL00302: Tankgutscheine 2008
      *
      *   Rev 2.1   Jan 18 2007 11:33:14   t13ae30
      *ASL00259: Tankgutscheine 2007
      *
      *   Rev 2.0   Jan 16 2007 17:30:40   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * ASL00259!16.01.07! 01 ! NEUERSTELLUNG             ! WILLIG     *
      * ASL00302!27.05.08! 02 ! ERW.VERSNR. 10 --> 15BYTE ! WILLIG     *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      * COBOL DECLARATION FOR VIEW VIVBA034                            *
      ******************************************************************
        05  WB-NI-VIVBA034-TAB.
           10 WB-NI-VIVBA034-ELE   PIC S9(04)  BINARY OCCURS 17 TIMES.
        05  WB-NI-VIVBA034         REDEFINES   WB-NI-VIVBA034-TAB.
           10 WB-NI-RINFO-VBNR            PIC S9(04)       BINARY.
           10 WB-NI-RINFO-VERSNR          PIC S9(04)       BINARY.
           10 WB-NI-RINFO-MONAT           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-RINFO-KZ              PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LEISTG1         PIC S9(04)       BINARY.
           10 WB-NI-PF-KZ-LEISTG1         PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LEISTG2         PIC S9(04)       BINARY.
           10 WB-NI-RINFO-NEUKDN-KZ       PIC S9(04)       BINARY.
           10 WB-NI-PF-KZ-LEISTG2         PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LEISTG3         PIC S9(04)       BINARY.
           10 WB-NI-PF-KZ-LEISTG3         PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LEISTG4         PIC S9(04)       BINARY.
           10 WB-NI-PF-KZ-LEISTG4         PIC S9(04)       BINARY.
           10 WB-NI-RINFO-BEMERKUNG       PIC S9(04)       BINARY.
        05  WB-VIVBA034.
           10 WB-RINFO-VBNR               PIC S9(9)        BINARY.
           10 WB-RINFO-VERSNR             PIC X(15).
           10 WB-RINFO-MONAT              PIC X(10).
           10 WB-KTY-WB-TYP               PIC S9(4)        BINARY.
           10 WB-KNR-WB-TYP               PIC S9(4)        BINARY.
           10 WB-RINFO-KZ                 PIC X(02).
           10 WB-RINFO-LAST-UPDATE        PIC X(26).
           10 WB-RINFO-LEISTG1            PIC X(10).
           10 WB-PF-KZ-LEISTG1            PIC X(01).
           10 WB-RINFO-LEISTG2            PIC X(10).
           10 WB-RINFO-NEUKDN-KZ          PIC X(01).
           10 WB-PF-KZ-LEISTG2            PIC X(01).
           10 WB-RINFO-LEISTG3            PIC X(10).
           10 WB-PF-KZ-LEISTG3            PIC X(01).
           10 WB-RINFO-LEISTG4            PIC X(10).
           10 WB-PF-KZ-LEISTG4            PIC X(01).
           10 WB-RINFO-BEMERKUNG          PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
