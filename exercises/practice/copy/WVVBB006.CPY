      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB006.CPv  $
      *
      *   Rev 2.1   Mar 27 2020 12:15:50   t13ex18
      *AVB01617: Neue Regionaliserte Direktionsbereiche für Allfina
      *nz 1 und 2 / Anpassung VB22 (VB-System)(+)
      *
      *   Rev 2.0   Mar 26 2020 17:13:36   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB006                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBB006 (DIR     )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    26.03.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01617!26.03.20!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBB006                          *
      *****************************************************************
 
        05 WB-NI-VIVBB006-TAB.
           10 WB-NI-VIVBB006-ELE   PIC S9(04)  BINARY OCCURS  15.
        05 WB-NI-VIVBB006          REDEFINES   WB-NI-VIVBB006-TAB.
           10 WB-NI-DIR-NR                PIC S9(04)       BINARY.
           10 WB-NI-DIR-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-DIR-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-DIR-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-REGION            PIC S9(04)       BINARY.
           10 WB-NI-KNR-REGION            PIC S9(04)       BINARY.
           10 WB-NI-KTY-RESERV-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-RESERV-TYP        PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-DIR-NAME              PIC S9(04)       BINARY.
           10 WB-NI-DIR-INFORMATION       PIC S9(04)       BINARY.
           10 WB-NI-KTY-VERTRBER          PIC S9(04)       BINARY.
           10 WB-NI-KNR-VERTRBER          PIC S9(04)       BINARY.
           10 WB-NI-KTY-REGDB             PIC S9(04)       BINARY.
           10 WB-NI-KNR-REGDB             PIC S9(04)       BINARY.
 
        05 WB-VIVBB006.
           10 WB-DIR-NR                   PIC S9(04)       BINARY.
           10 WB-DIR-LAST-UPDATE          PIC  X(26).
           10 WB-KTY-DIR-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-DIR-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-REGION               PIC S9(04)       BINARY.
           10 WB-KNR-REGION               PIC S9(04)       BINARY.
           10 WB-KTY-RESERV-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-RESERV-TYP           PIC S9(04)       BINARY.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-DIR-NAME                 PIC  X(40).
           10 WB-DIR-INFORMATION          PIC  X(150).
           10 WB-KTY-VERTRBER             PIC S9(04)       BINARY.
           10 WB-KNR-VERTRBER             PIC S9(04)       BINARY.
           10 WB-KTY-REGDB                PIC S9(04)       BINARY.
           10 WB-KNR-REGDB                PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *
      *****************************************************************

