      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0341.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0341.CPv  $
      *
      *   Rev 2.1   Jun 17 2020 09:42:22   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *copy und PI/PO-Bereich aus GENERCBL (BPR)
      *
      *   Rev 2.0   Jun 09 2020 12:43:56   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0341                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0341 (OFAUS   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    09.06.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-712 !09.06.20!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0341                          *
      *****************************************************************
 
        05 WB-NI-VIVB0341-TAB.
           10 WB-NI-VIVB0341-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0341          REDEFINES   WB-NI-VIVB0341-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-OFAUS-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-OFAUS-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-OFAUS-ANSPR-BETRAG    PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
 
        05 WB-VIVB0341.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-OFAUS-ANSPR-VOM          PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-OFAUS-LAST-UPDATE        PIC  X(26).
           10 WB-OFAUS-ANSPR-BETRAG       PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
