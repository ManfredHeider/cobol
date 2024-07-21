      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0181.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRVB0181.CPv  $
      *
      *   Rev 2.12   Dec 21 2020 17:33:54   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.11   Feb 13 2017 09:49:34   t13ae13
      *AVB01422: neuer VB-Vertrag 2017: UPVB0128 anpassen (SWL)
      *
      *   Rev 2.10   Jan 11 2013 12:18:52   t13ae13
      *AVB01256: DVAG-Wien: Einführung neuer VB-Vertrag 2012 - Host
      * - VB22
      *
      *   Rev 2.9   Jul 30 2008 10:08:48   t13ae13
      *FVB00669: Korrekturen im VB-System - VBA1/VBAK (swl)
      *
      *   Rev 2.8   25 Jun 2007 13:15:22   t13ae13
      *FVB00680: VB22 - Neuen VB-Vertrag 2007 überlesen
      *
      *   Rev 2.7   24 Apr 2007 15:54:04   t13ae27
      *AVB00908: Neuer VB-Vertrag 2007 - Prog
      *
      *   Rev 2.6   Sep 26 2003 15:52:06   t13ex22
      *AVB00481: Ermittlung frühest möglicher Kündigungstermin
      *
      *   Rev 2.5   Mar 31 2003 12:54:32   t13ae35
      *AVB00666 - neue Funktion DRVB0641
      *
      *   Rev 2.4   06 Nov 2000 12:55:04   T13AE30
      *ASL00048 - Neue Zusatzvereinb. wg. "Neuer VB-Vertrag"
      *
      *   Rev 2.3   Apr 18 2000 12:43:14   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.2   Feb 14 2000 16:02:36   T13AE42
      *AVB00474 - Anpassung neuer VB-Vertrag
      *
      *   Rev 2.1   May 19 1999 16:11:22   T13AE42
      *AAS00127 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:43:50   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   23 Jul 1997 12:11:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0181                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONEN FüR DAS LESE-OZP ZWVB0013 - AV                  *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!17.10.97!    ! NEU                       ! HEIDER   *
      * AAS00049!07.07.98! 01 ! NEUE FUNKTION DRVB0295    ! WALB     *
      * AAS00127!18.03.99! 02 ! NEUE FUNKTION DRVB0370    ! WALB     *
      * AVB00474!11.02.00! 03 ! NEUE FUNKTION DRVB0266    !SCHMIDT,B.*
      * ASL00046!30.03.00!2.3 ! Neue Funktion DRVB0468    ! T. Moll  *
      * ASL00048!06.11.00!2.4 ! NEUE Funktionen           ! TH.WILLIG*
      *         !        !    ! DRVB0510 + DRVB0513       !          *
      * AVB00666!07.03.03!2.5 ! NEUE FUNKTION DRVB0641    ! J.HERING *
      * ABV00481!09.09.03!2.6   NEUE FUNKTION DRVB0680    ! MUND     *
      * AVB00908!20.04.07!2.4 ! Neue Funktion DRVB0853    ! P.Duerr  *
      * FVB00680!25.06.07!2.7 ! NEUE FUNKTION DRVB0857    ! WALB     *
      * FVB00669!28.07.08!2.9 ! NEUE FUNKTION DRVB0910    ! WALB     *
      * AVB01256!30.11.12!2.10! NEUE FUNKTION DRVB1067    ! WALB     *
      * AVB01422!24.01.17!2.11! NEUE FUNKTION DRVB1158    ! WALB     *
      * VLA-988 !23.10.20!2.12! NEUE FUNKTION DRVB1294    ! T13AE30  *
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
       01  PI-ZWVB0013.
           05  PI-ZWVB0013-FUNKTION            PIC  X(008).
               88 PI-ZWVB0013-DRVB0182              VALUE 'DRVB0182'.
070798         88 PI-ZWVB0013-DRVB0295              VALUE 'DRVB0295'.
180399         88 PI-ZWVB0013-DRVB0370              VALUE 'DRVB0370'.
110200         88 PI-ZWVB0013-DRVB0266              VALUE 'DRVB0266'.
300300         88 PI-ZWVB0013-DRVB0468              VALUE 'DRVB0468'.
061100         88 PI-ZWVB0013-DRVB0510              VALUE 'DRVB0510'.
061100         88 PI-ZWVB0013-DRVB0513              VALUE 'DRVB0513'.
070303         88 PI-ZWVB0013-DRVB0641              VALUE 'DRVB0641'.
100903         88 PI-ZWVB0013-DRVB0680              VALUE 'DRVB0680'.
200407         88 PI-ZWVB0013-DRVB0853              VALUE 'DRVB0853'.
250607         88 PI-ZWVB0013-DRVB0857              VALUE 'DRVB0857'.
280708         88 PI-ZWVB0013-DRVB0910              VALUE 'DRVB0910'.
301112         88 PI-ZWVB0013-DRVB1067              VALUE 'DRVB1067'.
240117         88 PI-ZWVB0013-DRVB1158              VALUE 'DRVB1158'.
121020         88 PI-ZWVB0013-DRVB1294              VALUE 'DRVB1294'.
      *
      ******* ENDE COPY-MEMBER **************************************


