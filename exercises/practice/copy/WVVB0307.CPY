      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0307.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0307.CPv  $
      *
      *   Rev 2.1   Jun 22 2009 09:26:54   t13ex43
      *AAS00815: PGM2 - Neukundenwettbewerb
      *
      *   Rev 2.0   Jun 04 2009 11:11:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0307                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0307 (NKLOS   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.06.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00815!12.06.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0307                           *
      *****************************************************************

        05 WB-NI-VIVB0307-TAB.
           10 WB-NI-VIVB0307-ELE   PIC S9(04)  BINARY OCCURS   6.
        05 WB-NI-VIVB0307          REDEFINES   WB-NI-VIVB0307-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-NKLOS-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-NKLOS-LOSNR           PIC S9(04)       BINARY.
           10 WB-NI-NKLOS-LAST-UPDAT      PIC S9(04)       BINARY.
           10 WB-NI-NKLOS-KZ              PIC S9(04)       BINARY.
           10 WB-NI-NKLOS-GUELTIG         PIC S9(04)       BINARY.

        05 WB-VIVB0307.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-NKLOS-DAT-VON            PIC  X(10).
           10 WB-NKLOS-LOSNR              PIC S9(09)       BINARY.
           10 WB-NKLOS-LAST-UPDAT         PIC  X(26).
           10 WB-NKLOS-KZ                 PIC S9(04)       BINARY.
           10 WB-NKLOS-GUELTIG            PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *
      *****************************************************************
