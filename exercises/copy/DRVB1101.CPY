      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1101.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1101.CPv  $
      *
      *   Rev 2.1   May 21 2015 13:18:08   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   May 21 2015 12:57:16   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1101                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0125                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    21.05.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!21.05.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1101 - ERMITTELN SATZ AUS TABELLE USTAT       *
      *                        zu einem VB und einem Jahr             *
      *****************************************************************
 
       01 PI-DRVB1101.
          05 PI-DRVB1101-DATEN.
             10 PI-DRVB1101-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1101-USTAT-JAHR         PIC S9(04)       BINARY.
 
       01 PO-DRVB1101.
         03 PO-DRVB1101-RC                     PIC  X(03).
             88 PO-DRVB1101-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1101-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1101-DATEN.
          05 PO-DRVB1101-NI-VIVB0125.
             10 NI-DRVB1101-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1101-USTAT-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1101-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1101-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1101-USTAT-LAST-UPD     PIC S9(04)       BINARY.
             10 NI-DRVB1101-USTAT-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1101-USTAT-AEND-DURCH   PIC S9(04)       BINARY.
             10 NI-DRVB1101-ZUGANG             PIC S9(04)       BINARY.
             10 NI-DRVB1101-VON                PIC S9(04)       BINARY.
             10 NI-DRVB1101-BIS                PIC S9(04)       BINARY.
          05 PO-DRVB1101-DCL-VIVB0125.
             10 PO-DRVB1101-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1101-USTAT-DAT-VON      PIC  X(26).
             10 PO-DRVB1101-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1101-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1101-USTAT-LAST-UPD     PIC  X(26).
             10 PO-DRVB1101-USTAT-DAT-BIS      PIC  X(10).
             10 PO-DRVB1101-USTAT-AEND-DURCH   PIC  X(08).
             10 PO-DRVB1101-ZUGANG             PIC  X(26).
             10 PO-DRVB1101-VON                PIC  X(26).
             10 PO-DRVB1101-BIS                PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1101   --------------------------*
      *---------------------------------------------------------------*
 

