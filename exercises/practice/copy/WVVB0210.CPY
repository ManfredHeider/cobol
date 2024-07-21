      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0210.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0210.CPv  $
      *
      *   Rev 2.2   Sep 01 2011 11:47:54   t13ae14
      *ASL00390: Videosonderbonus Anpassung PMRW0018
      *
      *   Rev 2.1   Aug 19 2011 14:11:46   t13ae14
      *ASL00390: Videosonderbonus FTP und Load in die Tabelle VIDEO
      *A, Anpassung PMRW0018
      *
      *   Rev 2.0   Aug 12 2011 11:17:16   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: WVVB0210                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0210 (VIDEOA  )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    12.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00390!12.08.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0210                           *
      *****************************************************************

        05 WB-NI-VIVB0210-TAB.
           10 WB-NI-VIVB0210-ELE   PIC S9(04)  BINARY OCCURS  10.
        05 WB-NI-VIVB0210          REDEFINES   WB-NI-VIVB0210-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-VIDEOA-ANSPR-VOM      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-VIDEOA-LAST-UPDATE    PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-VIDEOA-ANSPR-BETRA    PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0210.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-VIDEOA-ANSPR-VOM         PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-VIDEOA-LAST-UPDATE       PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-VIDEOA-ANSPR-BETRA       PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

      *---------------------------------------------------------------*
