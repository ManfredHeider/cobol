      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0046.CPv  $
      *
      *   Rev 2.1   Jun 02 2005 16:43:44   t13ex43
      *ASL00204: PGM - K-Leistungszuschuss Zugriffsmodule
      *
      *   Rev 2.0   Jun 01 2005 14:28:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0046                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0046 (KLZA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    02.06.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00204!02.06.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0046                          *
      *****************************************************************
        05  WB-NI-VIVB0046-TAB.
           10 WB-NI-VIVB0046-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0046         REDEFINES   WB-NI-VIVB0046-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BEITRAG          PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0046.
           10 WB-MA-ST-NR                    PIC S9(09)       BINARY.
           10 WB-KLZA-ANSPR-VOM              PIC X(10).
           10 WB-KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 WB-KLZA-LAST-UPDATE            PIC X(26).
           10 WB-KLZA-BEITRAG                PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 WB-KLZA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS            PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
