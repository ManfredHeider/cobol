      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0977.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0977.CPv  $
      *
      *   Rev 2.1   Apr 06 2010 13:32:08   t13ex13
      *ASL00344: RGT: SL08
      *
      *   Rev 2.0   Mar 22 2010 15:50:02   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0977                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0078                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    22.03.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!22.03.10!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0977 - ERMITTELN KAND EINTRAG ZU CURRENT DATE *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0977.
          05 PI-DRVB0977-DATEN.
             10 PI-DRVB0977-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0977-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB0977-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.

       01 PO-DRVB0977.
         03 PO-DRVB0977-RC                     PIC  X(03).
             88 PO-DRVB0977-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0977-RC-NOT-FOUND            VALUE 'NFD'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0977   --------------------------*
      *---------------------------------------------------------------*
