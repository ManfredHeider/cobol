      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0225.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0225.CPv  $
      *
      *   Rev 2.2   May 02 2017 15:55:06   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.1   May 02 2017 13:55:10   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.0   Apr 28 2017 10:56:14   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0225                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0225 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    28.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!28.04.17!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0225                           *
      *****************************************************************
 
        05 WB-NI-VIVB0225-TAB.
           10 WB-NI-VIVB0225-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0225          REDEFINES   WB-NI-VIVB0225-TAB.
           10 WB-NI-FPRA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-FPRA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-FPRA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-FPRA-ANSP-BET-FREI    PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-FPRA-ANSP-BET-UPFL    PIC S9(04)       BINARY.
 
        05 WB-VIVB0225.
           10 WB-FPRA-ST-NR               PIC S9(09)       BINARY.
           10 WB-FPRA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-FPRA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-FPRA-ANSP-BET-FREI       PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-FPRA-ANSP-BET-UPFL       PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

