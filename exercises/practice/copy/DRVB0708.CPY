      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0708.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0708.CPv  $
      *
      *   Rev 2.3   Apr 18 2006 12:09:10   t13ae13
      *ASL00226: Dimension 7 Zukunft - MAP: OZP für MAPA
      *
      *   Rev 2.2   May 24 2004 12:31:40   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.1   May 11 2004 15:04:34   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.0   May 10 2004 14:07:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0708                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!10.05.04!2.1 ! NEU                       ! KLEEMEIER *
240504* ASL00183!24.05.04!2.2 ! NEUE FUNKTION DRVB0713    ! KLEEMEIER *
300306* ASL00226!30.03.06!2.3 ! Neue Funktionen DRVB0820, ! S. Walb   *
      *         !        !    ! DRVB0821, DRVB0822        !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZXVB0091.
           05  PI-ZXVB0091-FUNKTION            PIC  X(008).
               88 PI-ZXVB0091-DRVB0709              VALUE 'DRVB0709'.
240504         88 PI-ZXVB0091-DRVB0713              VALUE 'DRVB0713'.
300306         88 PI-ZXVB0091-DRVB0820              VALUE 'DRVB0820'.
300306         88 PI-ZXVB0091-DRVB0821              VALUE 'DRVB0821'.
100406         88 PI-ZXVB0091-DRVB0822              VALUE 'DRVB0822'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
