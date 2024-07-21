     *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0052.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0052.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:52:48   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Feb 27 1999 15:59:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:27:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0052                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0052 (EHNPB   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.07.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01332!08.07.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0052                           *
      *****************************************************************

        05 WB-NI-VIVB0052-TAB.
           10 WB-NI-VIVB0052-ELE   PIC S9(04)  BINARY OCCURS  14.
        05 WB-NI-VIVB0052          REDEFINES   WB-NI-VIVB0052-TAB.
           10 WB-NI-EHNPB-VS-ZEITRAUM     PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-BETR            PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-UNTST           PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-EBENE           PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-DAT-BIS         PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-ERSTELL-DAT     PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-EH-NEU-EIG      PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-NP-VON          PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-NP-BIS          PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-ABZPROZ-KL      PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-EH-SOLLANT      PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-EH-ABZUG-KL     PIC S9(04)       BINARY.
           10 WB-NI-EHNPB-BESTUEB-R-K     PIC S9(04)       BINARY.

        05 WB-VIVB0052.
           10 WB-EHNPB-VS-ZEITRAUM        PIC S9(04)       BINARY.
           10 WB-EHNPB-BETR               PIC S9(09)       BINARY.
           10 WB-EHNPB-UNTST              PIC S9(09)       BINARY.
           10 WB-EHNPB-EBENE              PIC  X(01).
           10 WB-EHNPB-DAT-VON            PIC  X(10).
           10 WB-EHNPB-DAT-BIS            PIC  X(10).
           10 WB-EHNPB-ERSTELL-DAT        PIC  X(10).
           10 WB-EHNPB-EH-NEU-EIG         PIC S9(09)V9(01) COMP-3.
           10 WB-EHNPB-NP-VON             PIC  X(10).
           10 WB-EHNPB-NP-BIS             PIC  X(10).
           10 WB-EHNPB-ABZPROZ-KL         PIC S9(03)V9(02) COMP-3.
           10 WB-EHNPB-EH-SOLLANT         PIC S9(09)V9(01) COMP-3.
           10 WB-EHNPB-EH-ABZUG-KL        PIC S9(09)V9(01) COMP-3.
           10 WB-EHNPB-BESTUEB-R-K        PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *
      *****************************************************************

