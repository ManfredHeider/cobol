      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0222.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0222.CPv  $
      *
      *   Rev 2.1   Mar 02 2015 12:42:52   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda 
      *Zugriffsmodule Tabelle AZA (TBVB0222)
      *
      *   Rev 2.0   Mar 02 2015 10:45:16   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0222                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0222 (AZA     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    27.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!27.02.15!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0222                           *
      *****************************************************************
 
        05 WB-NI-VIVB0222-TAB.
           10 WB-NI-VIVB0222-ELE   PIC S9(04)  BINARY OCCURS  10.
        05 WB-NI-VIVB0222          REDEFINES   WB-NI-VIVB0222-TAB.
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
 
        05 WB-VIVB0222.
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
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

