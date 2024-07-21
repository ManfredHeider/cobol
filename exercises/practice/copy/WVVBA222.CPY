      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA222.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA222.CPv  $
      *
      *   Rev 2.1   26 Jul 2017 18:22:32   t13ex71
      *ASL00567: LUCY2 für Ausbilderbonus - AZA (BPR)
      *
      *   Rev 2.0   26 Jul 2017 08:36:20   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA222                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA222 (AZA     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    27.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!26.07.17!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA222                           *
      *****************************************************************
 
        05 WB-NI-VIVBA222-TAB.
           10 WB-NI-VIVBA222-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVBA222          REDEFINES   WB-NI-VIVBA222-TAB.
           10 WB-NI-AZA-BETR-VBNR         PIC S9(04)       BINARY.
           10 WB-NI-AZA-ANSPR-MONAT       PIC S9(04)       BINARY.
           10 WB-NI-AZA-BETRAG            PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-AZA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-AZA-BETRAG-UPFL       PIC S9(04)       BINARY.
 
        05 WB-VIVBA222.
           10 WB-AZA-BETR-VBNR            PIC S9(09)       BINARY.
           10 WB-AZA-ANSPR-MONAT          PIC  X(10).
           10 WB-AZA-BETRAG               PIC S9(09)V9(02) COMP-3.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-AZA-LAST-UPDATE          PIC  X(26).
           10 WB-AZA-BETRAG-UPFL          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

