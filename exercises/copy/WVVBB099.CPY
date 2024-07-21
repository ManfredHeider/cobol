      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB099.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB099.CPv  $
      *
      *   Rev 2.1   May 09 2011 10:01:00   t13ex43
      *AVB01168: PGM - Pflege Orga-Betreuer (VB47)
      *
      *   Rev 2.0   Apr 28 2011 14:56:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB099                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB099 (NAMZ)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01168!02.05.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB099                           *
      *****************************************************************

        05 WB-NI-VIVBB099-TAB.
           10 WB-NI-VIVBB099-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVBB099          REDEFINES   WB-NI-VIVBB099-TAB.
           10 WB-NI-KTY-NAMZ-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-NAMZ-TYP          PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-LFD              PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANREDE            PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANREDE            PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-TITEL-AKAD       PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-NAME             PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-VORNAME          PIC S9(04)       BINARY.
           10 WB-NI-KTY-NAMZ-FUNK         PIC S9(04)       BINARY.
           10 WB-NI-KNR-NAMZ-FUNK         PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-NAMZ-DAT-BIS          PIC S9(04)       BINARY.

        05 WB-VIVBB099.
           10 WB-KTY-NAMZ-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-NAMZ-TYP             PIC S9(04)       BINARY.
           10 WB-NAMZ-LFD                 PIC S9(04)       BINARY.
           10 WB-KTY-ANREDE               PIC S9(04)       BINARY.
           10 WB-KNR-ANREDE               PIC S9(04)       BINARY.
           10 WB-NAMZ-TITEL-AKAD          PIC  X(10).
           10 WB-NAMZ-NAME                PIC  X(40).
           10 WB-NAMZ-VORNAME             PIC  X(20).
           10 WB-KTY-NAMZ-FUNK            PIC S9(04)       BINARY.
           10 WB-KNR-NAMZ-FUNK            PIC S9(04)       BINARY.
           10 WB-NAMZ-DAT-VON             PIC  X(26).
           10 WB-NAMZ-LAST-UPDATE         PIC  X(26).
           10 WB-NAMZ-DAT-BIS             PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
