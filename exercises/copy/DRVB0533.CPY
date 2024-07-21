      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0533.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0533.CPv  $
      *
      *   Rev 2.2   25 Jun 2001 07:52:24   t13ae13
      *AVB00534: Einführung PIN für VB-Portal 
      *
      *   Rev 2.1   09 May 2001 14:43:16   t13ae30
      * 
      *
      *   Rev 2.0   22 Feb 2001 16:59:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0533                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Lese-OZP ZWVB0081                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    09.05.2001                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!09.05.01! 2.1! NEU                       !TH.WILLIG *
      *         !13.06.01! 2.2! neue Funktion             !S. Walb   *
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
       01  PI-ZWVB0081.
           05  PI-ZWVB0081-FUNKTION            PIC  X(008).
               88 PI-ZWVB0081-DRVB0534              VALUE 'DRVB0534'.
130601         88 PI-ZWVB0081-DRVB0546              VALUE 'DRVB0546'.
      *
      ************************ ENDE COPY-MEMBER ************************
