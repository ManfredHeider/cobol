      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1108.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1108.CPv  $
      *
      *   Rev 2.3   Jan 07 2021 11:49:14   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.2   Nov 19 2020 07:53:38   t13ae56
      *AVB01644: PGM - KS2021: Anpassung Beförderungsdialog VB22
      *
      *   Rev 2.1   Jul 08 2015 12:33:34   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VB22
      *
      *   Rev 2.0   Jun 16 2015 13:09:08   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1108                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS PROGRAMM ZWVB0047                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    22.06.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!22.06.15!2.1 ! NEU                       ! S. Walb  *
290920* VLA-1004!29.09.20!2.2 ! DRVB1291 HINZU            ! KLEEMEIER*
141220* VLA-532 !14.12.20!2.3 ! DRVB1297 HINZU            ! KLEEMEIER*
      *         !        !    !                           !          *
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
       01  PI-ZWVB0047.
           05  PI-ZWVB0047-FUNKTION            PIC  X(008).
               88 PI-ZWVB0047-DRVB1109              VALUE 'DRVB1109'.
290920         88 PI-ZWVB0047-DRVB1291              VALUE 'DRVB1291'.
141220         88 PI-ZWVB0047-DRVB1297              VALUE 'DRVB1297'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *

