      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0569.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0569.CPv  $
      *
      *   Rev 2.4   Nov 04 2011 11:40:58   t13ae14
      *FVB00765: E-Mail von früher erneut erhalten
      *
      *   Rev 2.3   Nov 04 2011 10:35:12   t13ae14
      *FVB00765: E-Mail von früher erneut erhalten
      *
      *   Rev 2.2   Feb 20 2003 14:48:44   t13ex22
      *AAN00660: Erfassen eMail im VB43
      *
      *   Rev 2.1   Aug 09 2002 11:04:38   t13ae27
      *EMAIL/URL-Einführung
      *
      *   Rev 2.0   15 May 2001 13:33:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0569                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0083                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. DUERR.                                        *
      *    DATUM:    27.02.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00587!27.02.02! 01 ! NEU                       ! DUERR    *
      * AVB00660!13.02.03! 02 ! NEUE FUNKTION DRPA0636    ! MUND     *
      * FVB00765!03.11.11! 03 ! NEUE FUNKTION DRVB1031    ! OBER     *
      * VLA-5717!08.03.23! 04 ! NEUE FUNKTION DRVB0569    ! L. BETZ  * 
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
       01  PI-ZWVB0083.
           05  PI-ZWVB0083-FUNKTION            PIC  X(008).
               88 PI-ZWVB0083-DRVB0570              VALUE 'DRVB0570'.
               88 PI-ZWVB0083-DRVB0576              VALUE 'DRVB0576'.
               88 PI-ZWVB0083-DRVB0590              VALUE 'DRVB0590'.
               88 PI-ZWVB0083-DRVB0636              VALUE 'DRVB0636'.
               88 PI-ZWVB0083-DRVB1031              VALUE 'DRVB1031'.
080323         88 PI-ZWVB0083-DRVB1365              VALUE 'DRVB1365'.
      *
      ******* ENDE COPY-MEMBER DRVB0569 *****************************
