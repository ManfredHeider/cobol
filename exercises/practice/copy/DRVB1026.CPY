      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1026.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1026.CPv  $
      *
      *   Rev 2.2   May 31 2016 07:45:10   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.1   Sep 08 2011 15:53:44   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.0   Aug 05 2011 16:03:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1026                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0064                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00070!08.08.11!2.1 ! NEU                       ! KLEEMEIER *
250516* AVB01400!25.05.16!2.2 ! Neues Feld UST-Anteil     ! H. Wölflik*
      *         !        !    ! Neue View VIVBB064        !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1026 - ERMITTELN SATZ AUS TABELLE SLM         *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1026.
          05 PI-DRVB1026-DATEN.
             10 PI-DRVB1026-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1026-SLM-DAT-VON        PIC  X(10).
             10 PI-DRVB1026-KNR-SLM-TYP        PIC S9(04)       BINARY.
             10 PI-DRVB1026-SLM-TURNUS         PIC S9(09)       BINARY.
             10 PI-DRVB1026-SLM-HERKUNFT       PIC  X(01).
             10 PI-DRVB1026-SLM-VERARB-TYP     PIC  X(01).
 
       01 PO-DRVB1026.
         03 PO-DRVB1026-RC                     PIC  X(03).
             88 PO-DRVB1026-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1026-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1026-DATEN.
          05 PO-DRVB1026-NI-VIVBB064.
             10 NI-DRVB1026-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-DAT-VON        PIC S9(04)       BINARY.
             10 NI-DRVB1026-KNR-SLM-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-TURNUS         PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-HERKUNFT       PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-VERARB-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1026-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-LAST-UPDATE    PIC S9(04)       BINARY.
             10 NI-DRVB1026-KTY-SLM-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1026-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-AUSZAHLUNG     PIC S9(04)       BINARY.
             10 NI-DRVB1026-SLM-UST-ANTEIL     PIC S9(04)       BINARY.
          05 PO-DRVB1026-DCL-VIVBB064.
             10 PO-DRVB1026-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1026-SLM-DAT-VON        PIC  X(10).
             10 PO-DRVB1026-KNR-SLM-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1026-SLM-TURNUS         PIC S9(09)       BINARY.
             10 PO-DRVB1026-SLM-HERKUNFT       PIC  X(01).
             10 PO-DRVB1026-SLM-VERARB-TYP     PIC  X(01).
             10 PO-DRVB1026-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1026-SLM-LAST-UPDATE    PIC  X(26).
             10 PO-DRVB1026-KTY-SLM-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1026-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1026-SLM-AUSZAHLUNG     PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1026-SLM-UST-ANTEIL     PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1026   --------------------------*
      *---------------------------------------------------------------*

