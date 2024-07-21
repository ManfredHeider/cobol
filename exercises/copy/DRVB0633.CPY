      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0633.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0633.CPv  $
      *
      *   Rev 2.2   Oct 29 2009 13:19:10   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.1   Feb 20 2003 14:43:42   t13ex22
      *AAN00660: Erfassen eMail im VB43
      *
      *   Rev 2.0   Dec 04 2002 12:21:40   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0633
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0003  MMPZ (veraltet)       *
060709*    SCHNITTSTELLE FÜR DAS UPRO ZXVB0083  MEMZ                  *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    27.01.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00660!27.01.03! 01 ! NEU                       !MUND      *
      * AVB01014!06.07.09!2.2 ! umbenannt in ZXVB0083     !Walb      *
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
       01  PI-ZXVB0083.
           05 PI-ZXVB0083-FUNKTION            PIC X(008).
              88 PI-ZXVB0083-DRVB0634             VALUE 'DRVB0634'.

