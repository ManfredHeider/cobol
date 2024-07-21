      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1011.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 09:15:02   t13ex43
      *AVB01132: VB58 - Schliessen einer Direktionsleitung
      *
      *   Rev 2.0   Mar 09 2011 17:15:48   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1011                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0011                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.03.2011                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01132!10.03.11!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB1011.
           05  PI-DRVB1011-DATEN.
               10 PI-DRVB1011-BVH-BETR-NR      PIC S9(009) COMP.
      *
       01  PO-DRVB1011.
           05  PO-DRVB1011-RC                  PIC  X(003).
               88 PO-DRVB1011-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1011-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB1011-DATEN.
               10 PO-DRVB1011-ANZ-UNTST        PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB1011 ****************************************
      ****
