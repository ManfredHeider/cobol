      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0917.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0917.CPv  $
      *
      *   Rev 2.3   Nov 25 2009 13:42:00   t13ae30
      *AAS00841: Kapitäns-Club 2010
      *
      *   Rev 2.2   Mar 25 2009 10:08:24   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.1   Feb 20 2009 13:55:24   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Aug 28 2008 16:03:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0917                                      *
      *    --------------------------------------------------------   *
250309*    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0116 --> ZWVB0300       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.02.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!20.02.09!2.1 ! NEU                       ! KLEEMEIER *
250309* AAS00812!25.03.09!2.2 ! ZWVB0300 STATT ZWVB0116   ! KLEEMEIER *
060709* AAS00841!06.07.09!2.3 ! NEUE FUNKTION DRVB0946    ! TH.WILLIG *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
250309*01  PI-ZWVB0116.
250309**** 05  PI-ZWVB0116-FUNKTION            PIC  X(008).
250309****     88 PI-ZWVB0116-DRVB0928              VALUE 'DRVB0928'.
250309 01  PI-ZWVB0300.
250309     05  PI-ZWVB0300-FUNKTION            PIC  X(008).
250309         88 PI-ZWVB0300-DRVB0928              VALUE 'DRVB0928'.
060709         88 PI-ZWVB0300-DRVB0946              VALUE 'DRVB0946'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *
