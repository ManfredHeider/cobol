      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0109.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0109.CPv  $
      *
      *   Rev 2.3   Apr 05 2011 08:59:36   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.2   Feb 24 2005 10:53:58   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.1   Jul 13 1999 10:02:10   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.4   15 Sep 1998 17:17:42   T13AE35
      *ASL00022 - Aenderung, neue Fkt DRVB0313
      *
      *   Rev 1.3   26 Aug 1998 12:08:40   T13AE35
      *AAS00049 - Aenderung neue Fkt, DRVB0300
      *
      *   Rev 1.2   17 Apr 1998 16:25:38   T13EX30
      *AAS00005 - Anpassung
      *
      *   Rev 1.1   Apr 09 1997 16:56:02   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0109                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0018 - DZ                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: giese                                            *
      *    DATUM:    12.09.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.09.96!    ! NEU                       ! giese    *
      * AAS00005!16.12.97!    ! NEUE FUNKTION DRVB0203    ! SCHMIDT  *
      * ASL00022!21.07.98!    ! NEUE FUNKTION DRVB0300    ! WALB     *
      * ASL00022!24.08.98! 04 ! NEUE FUNKTION DRVB0313    ! WALB     *
      * AAS00051!13.07.99! 05 ! NEUE FUNKTION DRVB0419    ! WILLIG   *
      * AAS00513!23.02.05! 06 ! NEUE FUNKTION DRVB0756    ! P.Duerr  *
180211* AVB01132!18.02.11!2.3 ! NEUE FUNKTION DRVB1006    ! KLEEMEIER*
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
       01  PI-ZWVB0018.
           05  PI-ZWVB0018-FUNKTION            PIC  X(008).
               88 PI-ZWVB0018-DRVB0110              VALUE 'DRVB0110'.
161297         88 PI-ZWVB0018-DRVB0203              VALUE 'DRVB0203'.
210798         88 PI-ZWVB0018-DRVB0300              VALUE 'DRVB0300'.
240898         88 PI-ZWVB0018-DRVB0313              VALUE 'DRVB0313'.
130799         88 PI-ZWVB0018-DRVB0419              VALUE 'DRVB0419'.
230205         88 PI-ZWVB0018-DRVB0756              VALUE 'DRVB0756'.
180211         88 PI-ZWVB0018-DRVB1006              VALUE 'DRVB1006'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
