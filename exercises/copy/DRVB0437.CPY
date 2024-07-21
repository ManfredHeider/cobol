      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0437.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0437.CPv  $
      *
      *   Rev 2.2   28 Nov 2000 16:34:58   T13AE35
      *ASL00055 - neue Funktion DRVB0481
      *
      *   Rev 2.1   Apr 26 2000 12:36:30   T13AE35
      *ASL00049 - neu, Funktionencopy fuer
      *ZXVB0076
      *
      *   Rev 2.0   Aug 04 1999 10:30:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0437                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0076                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    11.04.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00049!11.04.00! 01 ! NEUERSTELLUNG             ! Walb     *
      * ASL00055!16.10.00! 02 ! Neue Funktion DRVB0481    ! Walb     *
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
       01  PI-ZXVB0076.
           05  PI-ZXVB0076-FUNKTION            PIC  X(008).
               88 PI-ZXVB0076-DRVB0464              VALUE 'DRVB0464'.
161000         88 PI-ZXVB0076-DRVB0481              VALUE 'DRVB0481'.
      *
      ******* ENDE COPY-MEMBER DRVB0348 *****************************
