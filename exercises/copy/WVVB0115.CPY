      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0115.CPv  $
      *
      *   Rev 2.1   Sep 10 2008 11:52:04   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.0   Aug 12 2008 07:28:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0115                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0115 (VSWVERS)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    12.08.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!12.08.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0115                          *
      *****************************************************************
        05  WB-NI-VIVB0115-TAB.
           10 WB-NI-VIVB0115-ELE   PIC S9(04)  BINARY OCCURS  4 TIMES.
        05  WB-NI-VIVB0115         REDEFINES   WB-NI-VIVB0115-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-VERS-NR               PIC S9(04)       BINARY.
           10 WB-NI-STATUS-KZ             PIC S9(04)       BINARY.
           10 WB-NI-VSWV-LAST-UPDATE      PIC S9(04)       BINARY.

        05  WB-VIVB0115.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-VERS-NR                  PIC S9(15)       COMP-3.
           10 WB-STATUS-KZ                PIC  X(02).
           10 WB-VSWV-LAST-UPDATE         PIC  X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
