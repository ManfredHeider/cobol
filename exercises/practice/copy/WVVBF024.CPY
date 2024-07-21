      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBF024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBF024.CPv  $
      *
      *   Rev 2.1   Nov 24 2011 08:43:14   t13ex43
      *ASL00405: PGM - VASP im VB32
      *
      *   Rev 2.0   Nov 17 2011 11:18:32   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBF024                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBF024 (VSWZ)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    18.11.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00405!18.11.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBF024                           *
      *****************************************************************

        05 WB-NI-VIVBF024-TAB.
           10 WB-NI-VIVBF024-ELE   PIC S9(04)  BINARY OCCURS  49.
        05 WB-NI-VIVBF024          REDEFINES   WB-NI-VIVBF024-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-ZUSAGE-DAT       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-IG-DAT         PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-GESAMT         PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-DAT-VON        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-STZ-JAHR       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VERS-SUM       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-STZ-ANZ        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VERS-NR        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VERS-SUM       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VERS-NR        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-DAT-VON        PIC S9(04)       BINARY.
           10 WB-NI-KTY-A-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-KNR-A-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-KTY-G-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-KNR-G-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-DBI-DAT          PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-DBI-KTO-NR       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-RV-VERS-NR     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-RV-RENTE       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-RV-ABF         PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-RV-BURENTE     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-RV-VERS-NR     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-RV-RENTE       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-RV-ABF         PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-RV-BURENTE     PIC S9(04)       BINARY.
           10 WB-NI-KTY-Z-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-KNR-Z-BERECHTIG       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VERSB-LV       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VERSB-RV       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VERSB-LV       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VERSB-RV       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-AGI-KTO-NR       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-AGI-KTO-NR2      PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VA-VERS-NR     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VA-VERSB       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VA-VSUM        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VA-RENTE       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-G-VA-BURENTE     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VA-VERS-NR     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VA-VERSB       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VA-VSUM        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VA-RENTE       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-Z-VA-BURENTE     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-VA-VERS-NR     PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-VA-VERSB       PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-VA-VSUM        PIC S9(04)       BINARY.
           10 WB-NI-VSWZ-A-VA-RENTE       PIC S9(04)       BINARY.

        05 WB-VIVBF024.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-VSWZ-LAST-UPDATE         PIC  X(26).
           10 WB-VSWZ-ZUSAGE-DAT          PIC  X(10).
           10 WB-VSWZ-A-IG-DAT            PIC  X(10).
           10 WB-VSWZ-A-GESAMT            PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-DAT-VON           PIC  X(10).
           10 WB-VSWZ-G-STZ-JAHR          PIC  X(10).
           10 WB-VSWZ-G-VERS-SUM          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-STZ-ANZ           PIC S9(04)       BINARY.
           10 WB-VSWZ-G-VERS-NR           PIC S9(15)       COMP-3.
           10 WB-VSWZ-Z-VERS-SUM          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VERS-NR           PIC S9(15)       COMP-3.
           10 WB-VSWZ-Z-DAT-VON           PIC  X(10).
           10 WB-KTY-A-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-KNR-A-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-KTY-G-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-KNR-G-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-VSWZ-DBI-DAT             PIC  X(10).
           10 WB-VSWZ-DBI-KTO-NR          PIC  X(09).
           10 WB-VSWZ-G-RV-VERS-NR        PIC S9(15)       COMP-3.
           10 WB-VSWZ-G-RV-RENTE          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-RV-ABF            PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-RV-BURENTE        PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-RV-VERS-NR        PIC S9(15)       COMP-3.
           10 WB-VSWZ-Z-RV-RENTE          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-RV-ABF            PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-RV-BURENTE        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-Z-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-KNR-Z-BERECHTIG          PIC S9(04)       BINARY.
           10 WB-VSWZ-G-VERSB-LV          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-VERSB-RV          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VERSB-LV          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VERSB-RV          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-AGI-KTO-NR          PIC  X(10).
           10 WB-VSWZ-AGI-KTO-NR2         PIC  X(10).
           10 WB-VSWZ-G-VA-VERS-NR        PIC S9(15)       COMP-3.
           10 WB-VSWZ-G-VA-VERSB          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-VA-VSUM           PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-VA-RENTE          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-G-VA-BURENTE        PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VA-VERS-NR        PIC S9(15)       COMP-3.
           10 WB-VSWZ-Z-VA-VERSB          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VA-VSUM           PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VA-RENTE          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-Z-VA-BURENTE        PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-A-VA-VERS-NR        PIC S9(15)       COMP-3.
           10 WB-VSWZ-A-VA-VERSB          PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-A-VA-VSUM           PIC S9(09)V9(02) COMP-3.
           10 WB-VSWZ-A-VA-RENTE          PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  49                    *
      *****************************************************************
