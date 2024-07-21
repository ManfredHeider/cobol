      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0722.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0722.CPv  $
      *
      *   Rev 2.1   Oct 07 2004 07:50:20   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten für D-Leiter
      *
      *   Rev 2.0   Jul 02 2004 15:59:06   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0722                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0092                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.07.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!12.07.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0722                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0722.
          05 PI-DRVB0722-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0722-MAPB-MONAT         PIC  X(010).
      *
       01 PO-DRVB0722.
          05 PO-DRVB0722-RC                 PIC  X(003).
             88 PO-DRVB0722-RC-OK                VALUE 'OK '.
             88 PO-DRVB0722-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0722-DATEN.
             10 PO-DRVB0722-MAPB-TUPEL-GESAMT    PIC  X(087).
      *
      ***** ENDE COPY-MEMBER DRVB0722 *****************************
      *
