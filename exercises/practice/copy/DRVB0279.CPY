      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0279.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0279.CPv  $
      *
      *   Rev 2.2   Jan 07 2021 11:48:52   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.1   13 Oct 2000 09:32:28   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Aug 1998 16:52:22   T13AE35
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:25:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0279                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0010                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    24.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!24.06.98! 01 ! NEU                       ! WALB     *
      * AKI00001!15.07.98! 01 ! Erweterung DRVB0267       !Zgur-Vujic*
      * AVB00490!28.06.00! 03 ! neue Funktionen DRVB0494, ! Walb     *
      *         !08.08.00!    ! DRVB0496, DRVB0497        !          *
      * VLA-532 !25.11.20!2.2 ! DRVB1302 HINZU            ! KLEEMEIER*
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
       01  PI-ZWVB0010.
           05  PI-ZWVB0010-FUNKTION            PIC  X(008).
               88 PI-ZWVB0010-DRVB0280              VALUE 'DRVB0280'.
               88 PI-ZWVB0010-DRVB0267              VALUE 'DRVB0267'.
280600         88 PI-ZWVB0010-DRVB0494              VALUE 'DRVB0494'.
080800         88 PI-ZWVB0010-DRVB0496              VALUE 'DRVB0496'.
080800         88 PI-ZWVB0010-DRVB0497              VALUE 'DRVB0497'.
251120         88 PI-ZWVB0010-DRVB1302              VALUE 'DRVB1302'.
      *
      ******* ENDE COPY-MEMBER DRVB0279 *****************************

