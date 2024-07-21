      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0675.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0675.CPv  $
      *
      *   Rev 2.1   Sep 26 2003 15:51:22   t13ex22
      *AVB00481: Ermittlung frühest möglicher Kündigungstermin
      *
      *   Rev 2.0   Feb 17 2003 10:59:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0675                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0132               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    02.09.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00481!02.09.03! 01 ! NEU                       ! MUND     *
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
       01  PI-UPVB0132.
           05  PI-UPVB0132-FUNKTION            PIC  X(008).
               88 PI-UPVB0132-DRVB0676              VALUE 'DRVB0676'.
      *
      ******* ENDE COPY-MEMBER **************************************
