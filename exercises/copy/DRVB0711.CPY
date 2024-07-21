      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0711.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0711.CPv  $
      *
      *   Rev 2.2   Apr 18 2006 11:56:00   t13ae13
      *ASL00226: Dimension 7 Zukunft - MAP: OZP für MAPA
      *
      *   Rev 2.1   May 11 2004 15:05:02   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.0   May 10 2004 14:07:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0711                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!10.05.04!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00226!28.03.06!2.2 ! Anpassung an neue Tabelle ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0711                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0711.
          05 PI-DRVB0711-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0711-MAP-ANSPR-VOM      PIC  X(010).
          05 PI-DRVB0711-MAP-STUFE          PIC  X(010).
          05 PI-DRVB0711-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
280306    05 PI-DRVB0711-MAPA-UNTST         PIC S9(009) BINARY.
      *
       01 PO-DRVB0711.
          05 PO-DRVB0711-RC                 PIC  X(003).
             88 PO-DRVB0711-RC-OK                VALUE 'OK '.
             88 PO-DRVB0711-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0711-DATEN.
280306*      10 PO-DRVB0711-MAPA-TUPEL-GESAMT    PIC  X(090).
280306       10 PO-DRVB0711-MAPA-TUPEL-GESAMT    PIC  X(096).
      *
      ***** ENDE COPY-MEMBER DRVB0711 *****************************
      *
