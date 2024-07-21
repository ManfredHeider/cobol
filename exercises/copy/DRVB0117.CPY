      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0117.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0117.CPv  $
      *
      *   Rev 2.5   Jan 05 2016 14:05:40   t13ae30
      *AVB01379: Neue Funktion ZWVB0001
      *
      *   Rev 2.4   Oct 06 2015 11:12:14   t13ae14
      *AVB01379: Neue Funktion ZWVB0001
      *
      *   Rev 2.3   10 May 2000 14:03:40   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.2   Feb 14 2000 16:05:04   T13AE42
      *AVB00474 - Anpassung neuer VB-Vertrag
      *
      *   Rev 2.1   Sep 02 1999 12:09:46   T13AE30
      *
      *
      *   Rev 2.0   Feb 27 1999 15:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 Apr 1998 08:50:06   T13AE35
      *AVB00399 - zwei neue Funktionen
      *
      *   Rev 1.1   Apr 09 1997 16:56:40   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0117                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    25.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.10.96! 01 ! NEU                       ! AUERBACH *
      *         !23.09.97! 02 ! Neue Funktion DRVB0180    ! Jaeger   *
      * AVB00399!23.03.98! 03 ! NEUE FUNKTION DRVB0219    ! HERING   *
      * AAS00051!24.08.99! 04 ! NEUE FUNKTION DRVB0441    ! WILLIG   *
      * AVB00474!14.02.00! 05 ! NEUE FUNKTION DRVB0270    !SCHMIDT,B.*
      * ASL00046!03.05.00! 06 ! NEUE FUNKTION DRVB0474    ! Walb     *
      * AVB01379!05.10.15! 2.4! Neue Funktion DRVB1119    ! Ober     *
      *         !        !    !                           !          *
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
       01  PI-ZWVB0001.
           05  PI-ZWVB0001-FUNKTION            PIC  X(008).
               88 PI-ZWVB0001-DRVB0118              VALUE 'DRVB0118'.
               88 PI-ZWVB0001-DRVB0180              VALUE 'DRVB0180'.
230398         88 PI-ZWVB0001-DRVB0219              VALUE 'DRVB0219'.
240899         88 PI-ZWVB0001-DRVB0441              VALUE 'DRVB0441'.
140200         88 PI-ZWVB0001-DRVB0270              VALUE 'DRVB0270'.
030500         88 PI-ZWVB0001-DRVB0474              VALUE 'DRVB0474'.
051015         88 PI-ZWVB0001-DRVB1119              VALUE 'DRVB1119'.
      *
      ******* ENDE COPY-MEMBER **************************************


