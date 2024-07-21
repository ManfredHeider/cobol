      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1087.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1087.CPv  $
      *
      *   Rev 2.2   Apr 28 2015 12:58:04   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.1   Mar 13 2015 10:00:40   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   Mar 13 2015 08:58:56   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1087                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0125                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    27.04.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!27.04.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1087 - ERMITTELN SATZ AUS TABELLE USTAT       *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1087.
          05 PI-DRVB1087-DATEN.
             10 PI-DRVB1087-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1087-USTAT-DAT-VON      PIC  X(26).
 
       01 PO-DRVB1087.
         03 PO-DRVB1087-RC                     PIC  X(03).
             88 PO-DRVB1087-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1087-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1087-DATEN.
          05 PO-DRVB1087-NI-VIVB0125.
             10 NI-DRVB1087-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1087-USTAT-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1087-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1087-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1087-USTAT-LAST-UPD     PIC S9(04)       BINARY.
             10 NI-DRVB1087-USTAT-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1087-USTAT-AEND-DURCH   PIC S9(04)       BINARY.
             10 NI-DRVB1087-ZUGANG             PIC S9(04)       BINARY.
             10 NI-DRVB1087-VON                PIC S9(04)       BINARY.
             10 NI-DRVB1087-BIS                PIC S9(04)       BINARY.
          05 PO-DRVB1087-DCL-VIVB0125.
             10 PO-DRVB1087-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1087-USTAT-DAT-VON      PIC  X(26).
             10 PO-DRVB1087-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1087-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1087-USTAT-LAST-UPD     PIC  X(26).
             10 PO-DRVB1087-USTAT-DAT-BIS      PIC  X(10).
             10 PO-DRVB1087-USTAT-AEND-DURCH   PIC  X(08).
             10 PO-DRVB1087-ZUGANG             PIC  X(26).
             10 PO-DRVB1087-VON                PIC  X(26).
             10 PO-DRVB1087-BIS                PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1087   --------------------------*
      *---------------------------------------------------------------*

