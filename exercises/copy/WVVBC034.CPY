      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBC034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBC034.CPv  $
      *
      *   Rev 2.2   May 11 2011 11:48:46   t13ae30
      *ASL00378: Update Storno-KZ von 'J' auf 'F' für bereits im WB
      *W='81' gekennzeichnete Verträge
      *
      *   Rev 2.1   Nov 09 2009 11:15:32   t13ex13
      *AAS00859: VSP/K-Wettbewerb 2009
      *
      *   Rev 2.0   Oct 14 2009 10:52:14   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * AAS00859!16.01.07!2.0 ! NEUERSTELLUNG             ! GUTFLEISCH *
      * ASL00378!11.02.11!2.1 ! FEHLENDES ATTRIBUT EINGEF.! TH.WILLIG  *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      * COBOL DECLARATION FOR VIEW VIVBC034                            *
      ******************************************************************
        05  WB-NI-VIVBC034-TAB.
           10 WB-NI-VIVBC034-ELE   PIC S9(04)  BINARY OCCURS 20 TIMES.
        05  WB-NI-VIVBC034         REDEFINES   WB-NI-VIVBC034-TAB.
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
110311     10 WB-NI-RINFO-STORNO-KZ       PIC S9(04)       BINARY.
           10 WB-NI-RINFO-LEISTG5         PIC S9(04)       BINARY.
           10 WB-NI-PF-KZ-LEISTG5         PIC S9(04)       BINARY.
        05  WB-VIVBC034.
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
110311     10 WB-RINFO-STORNO-KZ          PIC X(01).
           10 WB-RINFO-LEISTG5            PIC X(10).
           10 WB-PF-KZ-LEISTG5            PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
