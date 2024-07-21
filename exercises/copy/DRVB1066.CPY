      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1066.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1066.CPv  $
      *
      *   Rev 2.1   Jan 24 2013 12:13:54   t13ex57
      *AVB01257: Mailadresse für VM
      *
      *   Rev 2.0   Nov 21 2012 13:19:22   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1066                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0124                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    21.11.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01257!21.11.12!2.1 ! NEU                       ! BOLL      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1066 - ERMITTELN SATZ AUS TABELLE MAINFO      *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1066.
          05 PI-DRVB1066-DATEN.
             10 PI-DRVB1066-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1066-KNR-MAINFO-TYP     PIC S9(04)       BINARY.
      *      10 PI-DRVB1066-MAINFO-DAT-VON     PIC  X(26).

       01 PO-DRVB1066.
         03 PO-DRVB1066-RC                     PIC  X(03).
             88 PO-DRVB1066-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1066-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1066-DATEN.
          05 PO-DRVB1066-NI-VIVB0124.
             10 NI-DRVB1066-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1066-KTY-MAINFO-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1066-KNR-MAINFO-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1066-MAINFO-DAT-VON     PIC S9(04)       BINARY.
             10 NI-DRVB1066-MAINFO-LAST-UPD    PIC S9(04)       BINARY.
             10 NI-DRVB1066-MAINFO-DAT-BIS     PIC S9(04)       BINARY.
             10 NI-DRVB1066-MAINFO-INFO        PIC S9(04)       BINARY.
          05 PO-DRVB1066-DCL-VIVB0124.
             10 PO-DRVB1066-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1066-KTY-MAINFO-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1066-KNR-MAINFO-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1066-MAINFO-DAT-VON     PIC  X(26).
             10 PO-DRVB1066-MAINFO-LAST-UPD    PIC  X(26).
             10 PO-DRVB1066-MAINFO-DAT-BIS     PIC  X(10).
             10 PO-DRVB1066-MAINFO-INFO        PIC  X(80).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1066   --------------------------*
      *---------------------------------------------------------------*
