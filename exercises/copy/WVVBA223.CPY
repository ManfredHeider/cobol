      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA223.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA223.CPv  $
      *
      *   Rev 2.1   Aug 10 2017 17:05:38   t13ae13
      *ASL00567: LUCY2 für Eliteclubbonus SL13 (swl) 
      *
      *   Rev 2.0   Jul 12 2017 10:53:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA223                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA223 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    12.07.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!12.07.17!2.1 ! NEU                       ! WALB      *
120522* VLA-4763!12.05.22!2.2 ! Felddefinitionen COMP-3   ! U. Heim  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA223                           *
      *****************************************************************

        05 WB-NI-VIVBA223-TAB.
           10 WB-NI-VIVBA223-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVBA223          REDEFINES   WB-NI-VIVBA223-TAB.
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
           10 WB-NI-ECBA-PART-EH-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-ECBA-EH-UFREI-RECH    PIC S9(04)       BINARY.
           10 WB-NI-ECBA-EH-UPFL-RECH     PIC S9(04)       BINARY.
           10 WB-NI-ECBA-BETRAG-UPFL      PIC S9(04)       BINARY.

        05 WB-VIVBA223.
           10 WB-ECBA-ST-NR               PIC S9(09)       BINARY.
           10 WB-ECBA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-ECBA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
120522     10 WB-ECBA-PART-EH             PIC S9(10)V9(01) COMP-3.
           10 WB-ECBA-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-ECBA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
120522     10 WB-ECBA-PART-EH-UPFL        PIC S9(10)V9(01) COMP-3.
120522     10 WB-ECBA-EH-UFREI-RECH       PIC S9(10)V9(01) COMP-3.
120522     10 WB-ECBA-EH-UPFL-RECH        PIC S9(10)V9(01) COMP-3.
           10 WB-ECBA-BETRAG-UPFL         PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

