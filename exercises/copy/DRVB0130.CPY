      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0130.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0130.CPv  $
      *
      *   Rev 2.1   May 17 1999 16:37:40   T13AE42
      *AAS00096 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 09 1999 15:39:46   T13AE35
      *AAS00096 - neue Fkt
      *
      *   Rev 1.1   Mar 06 1997 15:14:20   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0130                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0022                        *
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
      * AAS00096!26.01.99! 02 ! NEUE FUNKTION DRVB0354    ! WALB     *
      * AAS00117!22.02.99! 02 ! NEUE FUNKTION DRVB0364    ! SCHMIDT,B*
      *         !        !    !                           !          *
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
       01  PI-ZWVB0022.
           05  PI-ZWVB0022-FUNKTION            PIC  X(008).
               88 PI-ZWVB0022-DRVB0131              VALUE 'DRVB0131'.
260199         88 PI-ZWVB0022-DRVB0354              VALUE 'DRVB0354'.
220299         88 PI-ZWVB0022-DRVB0364              VALUE 'DRVB0364'.
      *
      ******* ENDE COPY-MEMBER DRVB0130 *****************************
