      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0320.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0320.CPv  $
      *
      *   Rev 2.3   Dec 18 2003 15:17:22   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.2   30 Mar 2001 06:58:18   T13AE13
      *ASL00068 - ORG für GS-D analog ORG für RGS
      *
      *   Rev 2.1   Feb 22 2000 16:27:04   T13AE30
      *ASL00047 - Neue Funktion DRVB0429
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Feb 1999 14:07:54   T13AE42
      *ASL00025 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0320                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    15.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00002!15.09.98! 01 ! NEU                       ! WALB     *
      * ASL00047!09.02.00! 02 ! NEUE FUNKTION DRVB0429    ! WALB     *
      * ASL00068!13.02.01! 03 ! NEUE FUNKTION DRVB0520    ! WALB     *
      * AAS00443!02.12.03! 04 ! NEUE FUNKTION DRVB0692    ! P.Duerr  *
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
       01  PI-ZJVB0009.
           05 PI-ZJVB0009-FUNKTION            PIC X(008).
              88 PI-ZJVB0009-DRVB0321             VALUE 'DRVB0321'.
090200        88 PI-ZJVB0009-DRVB0429             VALUE 'DRVB0429'.
130201        88 PI-ZJVB0009-DRVB0520             VALUE 'DRVB0520'.
021203        88 PI-ZJVB0009-DRVB0692             VALUE 'DRVB0692'.
