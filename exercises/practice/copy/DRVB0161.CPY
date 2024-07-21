      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0161.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0161.CPv  $
      *
      *   Rev 2.3   13 Nov 2007 15:37:28   t13ae30
      *FVB00704: Abbruch im VB498225
      *
      *   Rev 2.2   Jul 21 1999 09:59:08   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Apr 21 1999 10:25:56   T13AE30
      *AAS00051 - Neuerstellung PMVB0212
      *
      *   Rev 2.0   Mar 19 1999 10:29:58   T13AE30
      * 
      *
      *   Rev 1.2   12 Aug 1998 16:55:16   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.1   10 Jun 1998 12:03:16   T13EX30
      *AAS00042 - Anpassung wegen DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   21 Jul 1997 16:47:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0161                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO zxvb0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MADERNER                                         *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0370!01.08.97! 01 ! NEUERSTELLUNG             ! MADERNER *
      * AAS00042!06.05.98! 02 ! NEUE FUNKTION DRVB0234    ! WALB     *
      * AAS00049!25.05.98! 03 ! NEUE FUNKTION DRVB0245    ! WALB     *
      * AAS00001!19.06.98! 04 ! NEUE FUNKTION DRVB0275    ! ZGUR-VUJIC
      * AAS00051!04.11.98! 05 ! NEUE FUNKTION DRVB0337    ! WILLIG
      *         !        !    ! NICHT BENUTZTE FUNKTION   !          *
      *         !        !    ! DRVB0275 ENTFERNT         !          *
      * AAS00051!19.07.99! 06 ! NEUE FUNKTION DRVB0420    ! WILLIG   *
      * FVB00704!13.11.07! 07 ! NEUE FUNKTION DRVB0872    ! WILLIG   *
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
       01  PI-ZXVB0001.
           05  PI-ZXVB0001-FUNKTION            PIC  X(008).
               88 PI-ZXVB0001-DRVB0162              VALUE 'DRVB0162'.
               88 PI-ZXVB0001-DRVB0234              VALUE 'DRVB0234'.
               88 PI-ZXVB0001-DRVB0245              VALUE 'DRVB0245'.
041198*        88 PI-ZXVB0001-DRVB0275              VALUE 'DRVB0275'.
041198         88 PI-ZXVB0001-DRVB0337              VALUE 'DRVB0337'.
190799         88 PI-ZXVB0001-DRVB0420              VALUE 'DRVB0420'.
131107         88 PI-ZXVB0001-DRVB0872              VALUE 'DRVB0872'.
      *
      ******* ENDE COPY-MEMBER DRVB0161 *****************************
