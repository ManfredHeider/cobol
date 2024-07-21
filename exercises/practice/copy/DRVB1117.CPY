      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1117.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:53:28   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Jul 08 2015 11:00:54   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1117                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0053                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.07.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01332!08.07.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1117 - ERMITTELN SATZ AUS TABELLE BVSP        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1117.
          05 PI-DRVB1117-DATEN.
             10 PI-DRVB1117-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 PI-DRVB1117-VB-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1117-VB-BEZUGS-NR       PIC S9(09)       BINARY.
             10 PI-DRVB1117-BETR-DAT-VON       PIC  X(10).

       01 PO-DRVB1117.
         03 PO-DRVB1117-RC                     PIC  X(03).
             88 PO-DRVB1117-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1117-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1117-DATEN.
          05 PO-DRVB1117-NI-VIVB0053.
             10 NI-DRVB1117-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 NI-DRVB1117-VB-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1117-VB-BEZUGS-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1117-BETR-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1117-BETR-DAT-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1117-ERSTELL-DATUM      PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-GES-NEU         PIC S9(04)       BINARY.
             10 NI-DRVB1117-KTY-EREIGNIS-ANF   PIC S9(04)       BINARY.
             10 NI-DRVB1117-KNR-EREIGNIS-ANF   PIC S9(04)       BINARY.
             10 NI-DRVB1117-KTY-EREIGNIS-ENDE  PIC S9(04)       BINARY.
             10 NI-DRVB1117-KNR-EREIGNIS-ENDE  PIC S9(04)       BINARY.
             10 NI-DRVB1117-FOLGE-EH-DAT-BIS   PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-ZUSCHLAG        PIC S9(04)       BINARY.
             10 NI-DRVB1117-ABZPROZ-REGEL-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-SOLLANTEIL-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-ABZUG-PROZ-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-ABZUG-BETR-W    PIC S9(04)       BINARY.
             10 NI-DRVB1117-PART-EH-DAT-VON    PIC S9(04)       BINARY.
             10 NI-DRVB1117-PART-EH-DAT-BIS    PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-PARTNER         PIC S9(04)       BINARY.
             10 NI-DRVB1117-PART-WERTEN        PIC S9(04)       BINARY.
             10 NI-DRVB1117-PART-WERT-AB-RGS   PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-GES-11          PIC S9(04)       BINARY.
             10 NI-DRVB1117-ABZPROZ-REGEL-11   PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-SOLLANTEIL-11   PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-ABZUG-PROZ-11   PIC S9(04)       BINARY.
             10 NI-DRVB1117-PART-EBENE         PIC S9(04)       BINARY.
             10 NI-DRVB1117-EH-EIG-NEU         PIC S9(04)       BINARY.
          05 PO-DRVB1117-DCL-VIVB0053.
             10 PO-DRVB1117-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 PO-DRVB1117-VB-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1117-VB-BEZUGS-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1117-BETR-DAT-VON       PIC  X(10).
             10 PO-DRVB1117-BETR-DAT-BIS       PIC  X(10).
             10 PO-DRVB1117-ERSTELL-DATUM      PIC  X(10).
             10 PO-DRVB1117-EH-GES-NEU         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-KTY-EREIGNIS-ANF   PIC S9(04)       BINARY.
             10 PO-DRVB1117-KNR-EREIGNIS-ANF   PIC S9(04)       BINARY.
             10 PO-DRVB1117-KTY-EREIGNIS-ENDE  PIC S9(04)       BINARY.
             10 PO-DRVB1117-KNR-EREIGNIS-ENDE  PIC S9(04)       BINARY.
             10 PO-DRVB1117-FOLGE-EH-DAT-BIS   PIC  X(10).
             10 PO-DRVB1117-EH-ZUSCHLAG        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-ABZPROZ-REGEL-NEU  PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1117-EH-SOLLANTEIL-NEU  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-EH-ABZUG-PROZ-NEU  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-EH-ABZUG-BETR-W    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-PART-EH-DAT-VON    PIC  X(10).
             10 PO-DRVB1117-PART-EH-DAT-BIS    PIC  X(10).
             10 PO-DRVB1117-EH-PARTNER         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-PART-WERTEN        PIC  X(01).
             10 PO-DRVB1117-PART-WERT-AB-RGS   PIC  X(01).
             10 PO-DRVB1117-EH-GES-11          PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-ABZPROZ-REGEL-11   PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1117-EH-SOLLANTEIL-11   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-EH-ABZUG-PROZ-11   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1117-PART-EBENE         PIC  X(01).
             10 PO-DRVB1117-EH-EIG-NEU         PIC S9(09)V9(01) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1117   --------------------------*
      *---------------------------------------------------------------*

