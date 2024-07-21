      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0491.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRVB0491.CPv  $
      *
      *   Rev 2.7   Sep 18 2017 13:48:08   t13ae35
      *ASL00570: DVAG-Wien: Sonderleistungen ohne Finanzierungseinh
      *eiten [VB] - Lese-OZP ZWVB0058 (jhe)
      *
      *   Rev 2.6   01 Nov 2007 10:14:16   t13ae35
      *ASL00270: Dimension 7 Zukunft - BOZ
      *
      *   Rev 2.5   30 Mar 2007 10:00:24   t13ae27
      *AAS00679: Jahreswettbewerb 2007 ...
      *
      *   Rev 2.4   Mar 14 2005 09:33:42   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.3   Dec 18 2003 15:29:48   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.2   23 Aug 2001 11:17:22   t13ae13
      *AVB00543: Riesterbereinigung
      *
      *   Rev 2.1   13 Oct 2000 09:34:44   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   19 May 2000 09:49:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0491                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0058                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    28.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!28.06.00! 01 ! NEU                       ! Walb     *
      * AVB00543!31.07.01! 02 ! Neue Funktion DRVB0539    ! Walb     *
      * AAS00443!10.12.03! 03 ! Neue Funktion DRVB0693    ! P.Duerr  *
      * AAS00531!01.02.04! 04 ! Neue Funktion DRVB0752,   ! P.Duerr  *
      *         !        !    ! neue Funktion DRVB0753    !          *
      * AAS00679!05.03.07! 05 ! Neue Funktion DRVB0843    ! Duerr    *
      * ASL00272!16.09.07! 06 ! Neue Funktion DRVB0867    ! J.Hering *
      * ASL00570!11.08.17! 07 ! Neue Funktion DRVB1175    ! J.Hering *
      * ASL00570!17.08.17! 08 ! DRVB1176 + DRVB1177       ! J.Hering *
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
       01  PI-ZWVB0058.
           05  PI-ZWVB0058-FUNKTION            PIC  X(008).
               88 PI-ZWVB0058-DRVB0492              VALUE 'DRVB0492'.
310701         88 PI-ZWVB0058-DRVB0539              VALUE 'DRVB0539'.
310701         88 PI-ZWVB0058-DRVB0693              VALUE 'DRVB0693'.
010205         88 PI-ZWVB0058-DRVB0752              VALUE 'DRVB0752'.
010205         88 PI-ZWVB0058-DRVB0753              VALUE 'DRVB0753'.
050307         88 PI-ZWVB0058-DRVB0843              VALUE 'DRVB0843'.
260907         88 PI-ZWVB0058-DRVB0867              VALUE 'DRVB0867'.
110817         88 PI-ZWVB0058-DRVB1175              VALUE 'DRVB1175'.
170817         88 PI-ZWVB0058-DRVB1176              VALUE 'DRVB1176'.
170817         88 PI-ZWVB0058-DRVB1177              VALUE 'DRVB1177'.
      *
      ******* ENDE COPY-MEMBER DRVB0491 *****************************

