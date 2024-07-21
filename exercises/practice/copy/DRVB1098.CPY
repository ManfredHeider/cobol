      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1098.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1098.CPv  $
      *
      *   Rev 2.1   May 18 2015 16:17:30   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   May 18 2015 14:33:44   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1098                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0125                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    18.05.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!18.05.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1098 - steuerl. Status zum Zeitpunkt          *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1098.
          05 PI-DRVB1098-DATEN.
             10 PI-DRVB1098-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1098-ZPKT               PIC  X(26).
 
       01 PO-DRVB1098.
         03 PO-DRVB1098-RC                     PIC  X(03).
             88 PO-DRVB1098-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1098-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1098-DATEN.
          05 PO-DRVB1098-DCL-VIVB0125.
             10 PO-DRVB1098-USTAT-DAT-VON      PIC  X(26).
             10 PO-DRVB1098-KNR-USTAT-TYP      PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1098   --------------------------*
      *---------------------------------------------------------------*
 

