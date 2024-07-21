      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0425.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0425.CPv  $
      *
      *   Rev 2.2   Jan 15 2003 09:29:16   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.1   Dec 21 1999 09:22:48   T13AE27
      *neue Funktion für ZXVB0011
      *
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0425                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS List-OZP ZXVB0011                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.08.1999                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!04.08.99! 01 ! NEU                       ! Walb     *
      * ASL00149!07.11.02! 02 ! neue Funktion DRVB0610    ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-ZXVB0011.
           05  PI-ZXVB0011-FUNKTION            PIC  X(008).
               88 PI-ZXVB0011-DRVB0426              VALUE 'DRVB0426'.
071102         88 PI-ZXVB0011-DRVB0610              VALUE 'DRVB0610'.
071102         88 PI-ZXVB0011-DRVB0620              VALUE 'DRVB0620'.
      *
      ******* ENDE COPY-MEMBER **************************************



