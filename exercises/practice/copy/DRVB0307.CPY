      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0307.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRVB0307.CPv  $
      *
      *   Rev 2.10   13 Jan 2020 09:26:06   t13ae56
      *AVB01581: PGM - Paket 2 - Dialoge SL16 + SL17
      *
      *   Rev 2.9   Mar 22 2013 15:37:16   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.8   Mar 22 2013 15:03:08   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.7   Nov 18 2009 15:46:16   t13ae36
      *AAS00829: Erweiterung Access-DB für Ö - Sperre Abrechnung
      *
      *   Rev 2.6   May 07 2009 14:55:42   t13ae36
      *AAS00829: Erweiterung Access-DB für Ö - Sperre Abrechnung
      *
      *   Rev 2.5   Nov 09 2005 15:19:34   t13ex13
      *AVB00820: PIN-SST Adressdaten auch für ausgeschiedene VB
      *
      *   Rev 2.4   May 18 2004 13:40:52   t13ae30
      *Verkaufsförderung Riesterprodukte
      *
      *   Rev 2.3   Jun 25 1999 08:20:38   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.2   Jun 25 1999 08:11:24   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Jun 02 1999 14:25:16   T13AE30
      *AAS00051 - Stammdaten DTA ---> Aachen
      *
      *   Rev 2.0   Feb 27 1999 15:58:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Sep 1998 17:21:14   T13AE35
      *ASL00022 - Neuerstellung
      *
      *   Rev 1.0   09 Jul 1998 09:57:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0307                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0021  BV                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    29.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00022!29.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      * AAS00051!25.06.99! 02 ! ÄNDERUNG                  ! WILLIG   *
      * AAS00467!05.05.04! 03 ! NEU FUNKTION DRVB0707     ! WILLIG   *
      * AVB00820!27.09.05!2.5 ! Neue Funktion DRVB0794    !Gutfleisch*
      * AAS00829!05.05.09!2.6 ! Neue Funktion DRVB0934    ! Franke   *
      * AAS00829!18.11.09!2.7 !      wieder entfernt      ! Franke   *
      * AVB01244!04.03.13!2.8 ! NEU FUNKTION DRVB1068     ! WILLIG   *
      * VBL-819 !20.09.19!2.10! DRVB1234 HINZU            ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0021.
           05 PI-ZWVB0021-FUNKTION            PIC X(008).
              88 PI-ZWVB0021-DRVB0308             VALUE 'DRVB0308'.
              88 PI-ZWVB0021-DRVB0388             VALUE 'DRVB0388'.
              88 PI-ZWVB0021-DRVB0707             VALUE 'DRVB0707'.
270905        88 PI-ZWVB0021-DRVB0794             VALUE 'DRVB0794'.
181109**      88 PI-ZWVB0021-DRVB0960             VALUE 'DRVB0960'.
              88 PI-ZWVB0021-DRVB1068             VALUE 'DRVB1068'.
              88 PI-ZWVB0021-DRVB1234             VALUE 'DRVB1234'.

