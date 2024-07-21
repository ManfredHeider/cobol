      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1105.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1105.CPv  $
      *
      *   Rev 2.1   May 29 2015 12:50:50   t13ae35
      *AVB01347: USt.pfl. Provisionen / VB-System - Lese-OZP SEMZ
      *
      *   Rev 2.0   May 29 2015 12:03:56   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1105                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0028                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    29.05.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!29.05.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1105 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1105.
          05 PI-DRVB1105-DATEN.
             10 PI-DRVB1105-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1105-KTY-SEMZ-TYP       PIC S9(04)       BINARY.
             10 PI-DRVB1105-KNR-SEMZ-TYP       PIC S9(04)       BINARY.
 
       01 PO-DRVB1105.
         03 PO-DRVB1105-RC                     PIC  X(03).
             88 PO-DRVB1105-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1105-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1105-DATEN.
          05 PO-DRVB1105-NI-VIVB0028.
             10 NI-DRVB1105-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1105-KTY-SEMZ-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1105-KNR-SEMZ-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1105-SEMZ-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1105-SEMZ-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1105-SEMZ-TEILNAHME     PIC S9(04)       BINARY.
          05 PO-DRVB1105-DCL-VIVB0028.
             10 PO-DRVB1105-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1105-KTY-SEMZ-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1105-KNR-SEMZ-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1105-SEMZ-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1105-SEMZ-DAT-VON       PIC  X(10).
             10 PO-DRVB1105-SEMZ-TEILNAHME     PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1105   --------------------------*
      *---------------------------------------------------------------*

