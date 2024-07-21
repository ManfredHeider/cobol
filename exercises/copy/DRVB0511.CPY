      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0511.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0511.CPv  $
      *
      *   Rev 2.3   Jan 22 2018 12:36:08   t13ae13
      *ASL00573: VSW 2017 (Abwicklung Versorgungsentgelte 2017) - U
      *PVB0093, UPVB0043, UPVB0117 (swl)
      *
      *   Rev 2.2   Jan 14 2003 09:27:56   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.1   23 Jan 2001 12:02:38   T13AE28
      *AVB00480: Ber.Eig.E für RD1-D
      *
      *   Rev 2.0   07 Sep 2000 11:32:14   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0511                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0117                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    20.11.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00480!20.11.00! 01 ! NEUERSTELLUNG             ! Walb     *
      * AVB00649!11.12.02!2.2 ! neue Funktion DRVB0484    ! Walb     *
      * ASL00573!10.01.18!2.3 ! neue Funktion DRVB1185    ! Walb     *
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
       01  PI-UPVB0117.
           05  PI-UPVB0117-FUNKTION            PIC  X(008).
               88 PI-UPVB0117-DRVB0512              VALUE 'DRVB0512'.
111202         88 PI-UPVB0117-DRVB0484              VALUE 'DRVB0484'.
100118         88 PI-UPVB0117-DRVB1185              VALUE 'DRVB1185'.
      *
      ******* ENDE COPY-MEMBER DRVB0511 *****************************

