      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0638.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0638.CPv  $
      *
      *   Rev 2.4   Sep 20 2004 10:02:10   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.3   Mar 26 2003 09:41:44   t13ex22
      *FVB00523: Neue Funktion DRVB0648
      *
      *   Rev 2.2   Mar 19 2003 08:45:46   t13ex22
      *AVB00648: Neue Funktion DRVB0645
      *
      *   Rev 2.1   Feb 20 2003 15:33:14   t13ex22
      *AVB00648: Unterscheidung VBA/ VBA-Karriere
      *
      *   Rev 2.0   Dec 04 2002 12:22:02   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0638                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0128               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    14.02.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00648!14.02.03! 01 ! NEU                       ! MUND     *
      * AVB00648!18.03.03! 2.1! NEUE FKT. DRVB0645        ! MUND     *
      * FVB00523!25.03.03! 2.2! NEUE FKT. DRVB0648        ! MUND     *
160804* AVD00001!16.08.04! 2.4! NEUE FUNKTION DRVB0726    ! KLEEMEIER*
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
       01  PI-UPVB0128.
           05  PI-UPVB0128-FUNKTION            PIC  X(008).
               88 PI-UPVB0128-DRVB0639              VALUE 'DRVB0639'.
               88 PI-UPVB0128-DRVB0645              VALUE 'DRVB0645'.
               88 PI-UPVB0128-DRVB0648              VALUE 'DRVB0648'.
160804         88 PI-UPVB0128-DRVB0726              VALUE 'DRVB0726'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
