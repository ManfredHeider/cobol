      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0798.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0798.CPv  $
      *
      *   Rev 2.1   Jun 21 2006 12:43:46   t13ae13
      *ASL00224: SL04 - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   Sep 28 2005 14:09:12   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0798                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0057                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    04.05.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00224!04.05.06!2.1 ! NEU                       ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0798                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0798.
          05 PI-DRVB0798-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0798-ABBA-ANSPR-VOM     PIC  X(010).
          05 PI-DRVB0798-ABBA-PART-VBNR     PIC S9(009) BINARY.
          05 PI-DRVB0798-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
      *
       01 PO-DRVB0798.
          05 PO-DRVB0798-RC                 PIC  X(003).
             88 PO-DRVB0798-RC-OK                VALUE 'OK '.
             88 PO-DRVB0798-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0798-DATEN.
             10 PO-DRVB0798-ABBA-TUPEL-GESAMT    PIC  X(116).
      *
      ***** ENDE COPY-MEMBER DRVB0798 *****************************
      *
