      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0132.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0132.CPv  $
      *
      *   Rev 2.3   Apr 05 2011 08:59:42   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.2   17 Oct 2000 13:36:02   T13AE30
      *FAN00264 - ANPASSUNG WG. ERMITTLUNG BD-STRUKTUR
      *
      *   Rev 2.1   May 17 1999 15:59:18   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 09 1999 11:58:20   T13AE35
      *AAS00095 - Aenderung neue Funktion
      *
      *   Rev 1.2   26 Aug 1998 12:15:18   T13AE35
      *AAS00049 - Aenderung, neue Fkt DRVB0311
      *
      *   Rev 1.1   Mar 06 1997 15:14:30   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0132                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0023                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    25.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.01.97! 01 ! NEU                       ! SCHÄFER  *
      * AAS00049!07.08.98! 02 ! NEUE FUNKTION DRVB0311    ! WALB     *
      * AAS00096!26.01.99! 03 ! NEUE FUNKTION DRVB0353    ! WALB     *
      * AAS00095!10.02.99! 04 ! NEUE FUNKTION DRVB0356    ! WALB     *
      * AAS00117!22.02.99! 05 ! NEUE FUNKTION DRVB0365    ! SCHMIDT,B*
      * FAN00264!16.10.00! 06 ! NEUE FUNKTION DRVB0509    ! TH.WILLIG*
180211* AVB01132!18.02.11!2.3 ! NEUE FUNKTION DRVB1008    ! KLEEMEIER*
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
       01  PI-ZWVB0023.
           05  PI-ZWVB0023-FUNKTION            PIC  X(008).
               88 PI-ZWVB0023-DRVB0133              VALUE 'DRVB0133'.
070898         88 PI-ZWVB0023-DRVB0311              VALUE 'DRVB0311'.
260199         88 PI-ZWVB0023-DRVB0353              VALUE 'DRVB0353'.
100299         88 PI-ZWVB0023-DRVB0356              VALUE 'DRVB0356'.
220299         88 PI-ZWVB0023-DRVB0365              VALUE 'DRVB0365'.
161000         88 PI-ZWVB0023-DRVB0509              VALUE 'DRVB0509'.
180211         88 PI-ZWVB0023-DRVB1008              VALUE 'DRVB1008'.
      *
      ******* ENDE COPY-MEMBER DRVB0132 *****************************
      *
