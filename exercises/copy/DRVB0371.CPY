      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0371.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0371.CPv  $
      *
      *   Rev 2.2   20 Mar 2001 10:20:26   T13AE13
      *AAS00264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.1   May 19 1999 16:12:32   T13AE42
      *AAS00127 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 17 1999 10:59:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0371                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    19.03.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00127!19.03.99! 01 ! NEUERSTELLUNG             ! WALB     *
      * AAS00264!14.03.01! 02 ! Neue Funktion DRVB0529    ! WALB     *
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
       01  PI-ZXVB0013.
           05  PI-ZXVB0013-FUNKTION            PIC  X(008).
               88 PI-ZXVB0013-DRVB0372              VALUE 'DRVB0372'.
               88 PI-ZXVB0013-DRVB0370              VALUE 'DRVB0370'.
140301         88 PI-ZXVB0013-DRVB0529              VALUE 'DRVB0529'.
      *
      ******* ENDE COPY-MEMBER DRVB0371 *****************************
