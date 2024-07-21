      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0339.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0339.CPv  $
      *
      *   Rev 2.2   Feb 22 2000 16:00:22   T13AE26
      *AVB00442 zurück auf produktive Version (Rev 2.0)
      *
      *   Rev 2.1   Feb 22 2000 15:57:38   T13AE26
      *AVB00442 geht NICHT in Produktion!
      *
      *   Rev 2.0   Apr 20 1999 16:33:24   T13EX31
      *AVB00396 neu
      *
      *
      *   Rev 1.0   27 Oct 1998 16:35:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0339                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0031                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Lee                                              *
      *    DATUM:    06.11.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !06.11.98! 01 ! NEUERSTELLUNG             ! Lee      *
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
      *    Die Daten sind in DRVB0340 übergeben.
      ****************************************************************
      *
       01  PI-ZXVB0031.
           05  PI-ZXVB0031-FUNKTION            PIC  X(008).
               88 PI-ZXVB0031-DRVB0340              VALUE 'DRVB0340'.
               88 PI-ZXVB0031-DRVB0342              VALUE 'DRVB0342'.
      *
      ******* ENDE COPY-MEMBER DRVB0339 *****************************
