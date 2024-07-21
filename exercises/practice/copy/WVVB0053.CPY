      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0053.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0053.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:52:42   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Feb 27 1999 15:59:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:27:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0053                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0053 (BVSP    )                        *
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
      * COBOL DEKLARATION FÜR VIEW VIVB0053                           *
      *****************************************************************

        05 WB-NI-VIVB0053-TAB.
           10 WB-NI-VIVB0053-ELE   PIC S9(04)  BINARY OCCURS  28.
        05 WB-NI-VIVB0053          REDEFINES   WB-NI-VIVB0053-TAB.
           10 WB-NI-VS-ZEITRAUM           PIC S9(04)       BINARY.
           10 WB-NI-VB-NR                 PIC S9(04)       BINARY.
           10 WB-NI-VB-BEZUGS-NR          PIC S9(04)       BINARY.
           10 WB-NI-BETR-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-BETR-DAT-BIS          PIC S9(04)       BINARY.
           10 WB-NI-ERSTELL-DATUM         PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-NEU            PIC S9(04)       BINARY.
           10 WB-NI-KTY-EREIGNIS-ANF      PIC S9(04)       BINARY.
           10 WB-NI-KNR-EREIGNIS-ANF      PIC S9(04)       BINARY.
           10 WB-NI-KTY-EREIGNIS-ENDE     PIC S9(04)       BINARY.
           10 WB-NI-KNR-EREIGNIS-ENDE     PIC S9(04)       BINARY.
           10 WB-NI-FOLGE-EH-DAT-BIS      PIC S9(04)       BINARY.
           10 WB-NI-EH-ZUSCHLAG           PIC S9(04)       BINARY.
           10 WB-NI-ABZPROZ-REGEL-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EH-SOLLANTEIL-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EH-ABZUG-PROZ-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EH-ABZUG-BETR-W       PIC S9(04)       BINARY.
           10 WB-NI-PART-EH-DAT-VON       PIC S9(04)       BINARY.
           10 WB-NI-PART-EH-DAT-BIS       PIC S9(04)       BINARY.
           10 WB-NI-EH-PARTNER            PIC S9(04)       BINARY.
           10 WB-NI-PART-WERTEN           PIC S9(04)       BINARY.
           10 WB-NI-PART-WERT-AB-RGS      PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-11             PIC S9(04)       BINARY.
           10 WB-NI-ABZPROZ-REGEL-11      PIC S9(04)       BINARY.
           10 WB-NI-EH-SOLLANTEIL-11      PIC S9(04)       BINARY.
           10 WB-NI-EH-ABZUG-PROZ-11      PIC S9(04)       BINARY.
           10 WB-NI-PART-EBENE            PIC S9(04)       BINARY.
           10 WB-NI-EH-EIG-NEU            PIC S9(04)       BINARY.

        05 WB-VIVB0053.
           10 WB-VS-ZEITRAUM              PIC S9(04)       BINARY.
           10 WB-VB-NR                    PIC S9(09)       BINARY.
           10 WB-VB-BEZUGS-NR             PIC S9(09)       BINARY.
           10 WB-BETR-DAT-VON             PIC  X(10).
           10 WB-BETR-DAT-BIS             PIC  X(10).
           10 WB-ERSTELL-DATUM            PIC  X(10).
           10 WB-EH-GES-NEU               PIC S9(09)V9(01) COMP-3.
           10 WB-KTY-EREIGNIS-ANF         PIC S9(04)       BINARY.
           10 WB-KNR-EREIGNIS-ANF         PIC S9(04)       BINARY.
           10 WB-KTY-EREIGNIS-ENDE        PIC S9(04)       BINARY.
           10 WB-KNR-EREIGNIS-ENDE        PIC S9(04)       BINARY.
           10 WB-FOLGE-EH-DAT-BIS         PIC  X(10).
           10 WB-EH-ZUSCHLAG              PIC S9(09)V9(01) COMP-3.
           10 WB-ABZPROZ-REGEL-NEU        PIC S9(03)V9(02) COMP-3.
           10 WB-EH-SOLLANTEIL-NEU        PIC S9(09)V9(01) COMP-3.
           10 WB-EH-ABZUG-PROZ-NEU        PIC S9(09)V9(01) COMP-3.
           10 WB-EH-ABZUG-BETR-W          PIC S9(09)V9(01) COMP-3.
           10 WB-PART-EH-DAT-VON          PIC  X(10).
           10 WB-PART-EH-DAT-BIS          PIC  X(10).
           10 WB-EH-PARTNER               PIC S9(09)V9(01) COMP-3.
           10 WB-PART-WERTEN              PIC  X(01).
           10 WB-PART-WERT-AB-RGS         PIC  X(01).
           10 WB-EH-GES-11                PIC S9(09)V9(01) COMP-3.
           10 WB-ABZPROZ-REGEL-11         PIC S9(03)V9(02) COMP-3.
           10 WB-EH-SOLLANTEIL-11         PIC S9(09)V9(01) COMP-3.
           10 WB-EH-ABZUG-PROZ-11         PIC S9(09)V9(01) COMP-3.
           10 WB-PART-EBENE               PIC  X(01).
           10 WB-EH-EIG-NEU               PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  28                    *
      *****************************************************************

