      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0137.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0137.CPv  $
      *
      *   Rev 2.2   Nov 02 1999 12:29:26   T13AE42
      *AOP00177 - neue Funktion DRVB0250
      *
      *   Rev 2.1   Jun 04 1999 13:01:48   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   26 Aug 1998 10:25:34   T13AE35
      *AAS00049 - Aenderung, neue Fkt. DRVB0297
      *
      *   Rev 1.1   Mar 06 1997 15:14:48   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0137                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0016                        *
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
      * AAS00049!06.07.98! 02 ! NEUE FUNKTION DRVB0297    ! WALB     *
      *         !        !    !                           !          *
      * AOP00177!31.05.99! 03 ! NEUE FUNKTION DRVB0404    ! SCHMIDT  *
      * AOP00177!02.11.99! 04 ! NEUE FUNKTION DRVB0250    ! SCHMIDT  *
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
       01  PI-ZWVB0016.
           05  PI-ZWVB0016-FUNKTION            PIC  X(008).
               88 PI-ZWVB0016-DRVB0138              VALUE 'DRVB0138'.
060798         88 PI-ZWVB0016-DRVB0297              VALUE 'DRVB0297'.
310599         88 PI-ZWVB0016-DRVB0404              VALUE 'DRVB0404'.
021199         88 PI-ZWVB0016-DRVB0250              VALUE 'DRVB0250'.
      *
      ******* ENDE COPY-MEMBER DRVB0137 *****************************
