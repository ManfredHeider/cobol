      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0601.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/cpy/DRVB0601.CPv  $
      *
      *   Rev 2.3   Feb 11 2003 11:54:38   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *
      *   Rev 2.2   Jan 14 2003 09:25:58   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.1   Nov 15 2002 16:57:18   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.0   Nov 05 2002 11:34:42   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0601                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0002                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    05.11.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00643!05.11.02! 01 ! NEU                       ! WILLIG   *
      * AVB00649!22.11.02!2.2 ! neue Funktionen DRVB0380, ! Walb     *
      *         !        !    ! DRVB0628                  !          *
      * AAS00387!10.02.03!2.3 ! neue Funktionen DRVB0635  ! U. BOLL  *
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
       01  PI-ZJVB0002.
           05  PI-ZJVB0002-FUNKTION            PIC  X(008).
               88 PI-ZJVB0002-DRVB0602              VALUE 'DRVB0602'.
               88 PI-ZJVB0002-DRVB0603              VALUE 'DRVB0603'.
               88 PI-ZJVB0002-DRVB0380              VALUE 'DRVB0380'.
               88 PI-ZJVB0002-DRVB0628              VALUE 'DRVB0628'.
               88 PI-ZJVB0002-DRVB0635              VALUE 'DRVB0635'.
      *
      ******* ENDE COPY-MEMBER **************************************
