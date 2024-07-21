      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0632.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0632.CPv  $
      *
      *   Rev 2.1   Jun 21 2006 13:10:06   t13ae13
      *ASL00224: SL04 - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   May 26 2006 10:40:46   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0632                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0057                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    26.05.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00224!26.05.06!2.1 ! NEU                       ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0632                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0632.
          05 PI-DRVB0632-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0632-ABBA-ANSPR-VOM     PIC  X(010).
      *
       01 PO-DRVB0632.
          05 PO-DRVB0632-RC                 PIC  X(003).
             88 PO-DRVB0632-RC-OK                VALUE 'OK '.
             88 PO-DRVB0632-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0632-DATEN.
             10 PO-DRVB0632-ABBA-PART-VBNR     PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0632 *****************************
      *
