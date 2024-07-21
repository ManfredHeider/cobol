      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0903.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0903.CPv  $
      *
      *   Rev 2.2   Nov 02 2016 09:51:48   t13ex30
      *ASL00559: Betreuung Versorgungsleistungen 2016 - PMSL0068 un
      *d PMSL0109 (HWO)
      *
      *   Rev 2.1   Sep 09 2008 12:04:50   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 1)
      *
      *   Rev 2.0   Jun 26 2008 14:04:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0903                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0113                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.06.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!27.06.08!2.1 ! NEU                       ! KLEEMEIER *
111016* ASL00559!11.10.16!2.2 ! Neue Funktion DRVB1151    ! H. Wölflik*
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZWVB0113.
           05  PI-ZWVB0113-FUNKTION            PIC  X(008).
               88 PI-ZWVB0113-DRVB0904              VALUE 'DRVB0904'.
               88 PI-ZWVB0113-DRVB0905              VALUE 'DRVB0905'.
               88 PI-ZWVB0113-DRVB0907              VALUE 'DRVB0907'.
               88 PI-ZWVB0113-DRVB0913              VALUE 'DRVB0913'.
111016         88 PI-ZWVB0113-DRVB1151              VALUE 'DRVB1151'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *

