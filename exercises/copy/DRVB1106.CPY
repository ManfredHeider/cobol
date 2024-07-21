      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1106.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1106.CPv  $
      *
      *   Rev 2.1   May 29 2015 12:50:54   t13ae35
      *AVB01347: USt.pfl. Provisionen / VB-System - Lese-OZP SEMZ
      *
      *   Rev 2.0   May 29 2015 12:04:02   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1106                                      *
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
      *    FUNKTION DRVB1106 - ERMITTELN ob Seminar vorliegt          *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1106.
          05 PI-DRVB1106-DATEN.
             10 PI-DRVB1106-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1106-KTY-SEMZ-TYP       PIC S9(04)       BINARY.
             10 PI-DRVB1106-KNR-SEMZ-TYP       PIC S9(04)       BINARY.
 
       01 PO-DRVB1106.
         03 PO-DRVB1106-RC                     PIC  X(03).
             88 PO-DRVB1106-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1106-RC-NOT-FOUND            VALUE 'NFD'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1106   --------------------------*
      *---------------------------------------------------------------*

