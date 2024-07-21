      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1115.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:53:14   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Jul 08 2015 11:00:38   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1115                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0052                    *
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
      *    FUNKTION DRVB1115 - ERMITTELN SATZ AUS TABELLE EHNPB       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1115.
          05 PI-DRVB1115-DATEN.
             10 PI-DRVB1115-EHNPB-VS-ZEITRAUM  PIC S9(04)       BINARY.
             10 PI-DRVB1115-EHNPB-BETR         PIC S9(09)       BINARY.
             10 PI-DRVB1115-EHNPB-UNTST        PIC S9(09)       BINARY.
             10 PI-DRVB1115-EHNPB-EBENE        PIC  X(01).

       01 PO-DRVB1115.
         03 PO-DRVB1115-RC                     PIC  X(03).
             88 PO-DRVB1115-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1115-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1115-DATEN.
          05 PO-DRVB1115-NI-VIVB0052.
             10 NI-DRVB1115-EHNPB-VS-ZEITRAUM  PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-BETR         PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-UNTST        PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-EBENE        PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-ERSTELL-DAT  PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-EH-NEU-EIG   PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-NP-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-NP-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-ABZPROZ-KL   PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-EH-SOLLANT   PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-EH-ABZUG-KL  PIC S9(04)       BINARY.
             10 NI-DRVB1115-EHNPB-BESTUEB-R-K  PIC S9(04)       BINARY.
          05 PO-DRVB1115-DCL-VIVB0052.
             10 PO-DRVB1115-EHNPB-VS-ZEITRAUM  PIC S9(04)       BINARY.
             10 PO-DRVB1115-EHNPB-BETR         PIC S9(09)       BINARY.
             10 PO-DRVB1115-EHNPB-UNTST        PIC S9(09)       BINARY.
             10 PO-DRVB1115-EHNPB-EBENE        PIC  X(01).
             10 PO-DRVB1115-EHNPB-DAT-VON      PIC  X(10).
             10 PO-DRVB1115-EHNPB-DAT-BIS      PIC  X(10).
             10 PO-DRVB1115-EHNPB-ERSTELL-DAT  PIC  X(10).
             10 PO-DRVB1115-EHNPB-EH-NEU-EIG   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1115-EHNPB-NP-VON       PIC  X(10).
             10 PO-DRVB1115-EHNPB-NP-BIS       PIC  X(10).
             10 PO-DRVB1115-EHNPB-ABZPROZ-KL   PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1115-EHNPB-EH-SOLLANT   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1115-EHNPB-EH-ABZUG-KL  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1115-EHNPB-BESTUEB-R-K  PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1115   --------------------------*
      *---------------------------------------------------------------*

