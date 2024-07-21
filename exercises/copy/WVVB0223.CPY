      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0223.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0223.CPv  $
      *
      *   Rev 2.1   Mar 04 2015 13:24:02   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda 
      *Zugriffsmodule Tabelle ECBA (TBVB0223)
      *
      *   Rev 2.0   Mar 03 2015 18:16:08   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0223                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0223 (ECBA    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    03.03.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!03.03.15!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0223                           *
      *****************************************************************
 
        05 WB-NI-VIVB0223-TAB.
           10 WB-NI-VIVB0223-ELE   PIC S9(04)  BINARY OCCURS  12.
        05 WB-NI-VIVB0223          REDEFINES   WB-NI-VIVB0223-TAB.
           10 WB-NI-ECBA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-ECBA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-ECBA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-ECBA-PART-EH          PIC S9(04)       BINARY.
           10 WB-NI-ECBA-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-ECBA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
 
        05 WB-VIVB0223.
           10 WB-ECBA-ST-NR               PIC S9(09)       BINARY.
           10 WB-ECBA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-ECBA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-ECBA-PART-EH             PIC S9(09)V9(01) COMP-3.
           10 WB-ECBA-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-ECBA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

