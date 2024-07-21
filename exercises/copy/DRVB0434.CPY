      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0434.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRVB0434.CPv  $
      *
      *   Rev 2.14   May 03 2016 13:16:52   t13ex30
      *AVB01400: Projekt Lucy2 - ZWVB0065
      *
      *   Rev 2.13   Oct 16 2015 11:39:14   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.12   Oct 13 2015 16:51:56   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.11   Jul 20 2010 12:18:48   t13ex57
      *ASL00352: BOZ DVAG
      *
      *   Rev 2.10   03 Sep 2007 14:45:56   t13ex57
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung Sch
      *reibmodul
      *
      *   Rev 2.9   Feb 22 2005 11:48:58   t13ex22
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005
      *. - Gesamt-EH bei ND berücksichtigt
      *
      *   Rev 2.8   Aug 12 2004 11:44:24   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.7   Dec 18 2003 15:41:10   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.6   Jun 10 2003 14:19:44   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.5   Feb 04 2003 10:48:34   t13ae27
      *Anpassungen für 2002 wg. Siegeradlerauswertung
      *
      *   Rev 2.4   12 Mar 2001 07:38:36   T13AE13
      *ASL00068 - OLZ I für RGS - D mit neuen Bedingungen
      *
      *   Rev 2.3   13 Oct 2000 09:33:00   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.2   Mar 03 2000 13:11:26   T13AE27
      *Neue Funktion DRVB0430
      *
      *
      *   Rev 2.1   Dec 21 1999 09:39:52   T13AE27
      *neue Funktion für ZWVB0065
      *
      *   Rev 2.0   Aug 04 1999 10:30:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0434                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Lese-OZP ZWVB0065                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    12.08.1999                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!12.08.99! 01 ! NEU                       ! Walb     *
      * ASL00047!22.02.00! 02 ! Neue Funktion DRVB0430    ! Walb     *
      * AVB00490!30.06.00! 03 ! Neue Funktion DRVB0495    ! Walb     *
      * ASL00068!09.02.01! 04 ! Neue Funktionen DRVB0516, ! Walb     *
      *         !        !    ! DRVB0517, DRVB0518        !          *
      * AVB00333!18.10.02! 05 ! Neue Funktion DRVB0606    ! P.Duerr  *
      * AVB00666!23.04.03! 06 ! NEUE FUNKTION DRVB0657    ! MUND     *
      * AAS00443!17.12.03! 07 ! NEUE FUNKTION DRVB0695    ! P.DUERR  *
      * ASL00725!09.07.04! 08 ! NEUE FUNKTION DRVB0725    ! MUND     *
      * AAS00513!21.02.05! 09 ! NEUE FUNKTION DRVB0755    ! MUND     *
      * ARW00041!30.08.07! 10 ! NEUE FUNKTION DRVB0865    !U. Boll   *
      * ASL00352!20.07.10! 11 ! NEUE FUNKTION DRVB0982    !U. Boll   *
      * AVB01347!03.09.15! 12 ! NEUE FUNKTION DRVB1118    !B.Pretsch *
      * AVB01347!14.10.15!2.13! Neue Funktionen DRVB1120 -! Walb     *
      *         !        !    ! DRVB1123                  !          *
      * AVB01400!14.04.16!2.14! Neue Funktion DRVB1130    !H.Wölflik *
      * AVB01400!21.04.16!2.14! Neue Funktionen DRVB1131- !H.Wölflik *
      *         !        !    !                 DRVB1133  !          *
      * AVB01400!29.04.16!2.14! Neue Funktion   DRVB1136  !H.Wölflik *
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
       01  PI-ZWVB0065.
           05  PI-ZWVB0065-FUNKTION            PIC  X(008).
               88 PI-ZWVB0065-DRVB0435              VALUE 'DRVB0435'.
               88 PI-ZWVB0065-DRVB0436              VALUE 'DRVB0436'.
220200         88 PI-ZWVB0065-DRVB0430              VALUE 'DRVB0430'.
300600         88 PI-ZWVB0065-DRVB0495              VALUE 'DRVB0495'.
090201         88 PI-ZWVB0065-DRVB0516              VALUE 'DRVB0516'.
090201         88 PI-ZWVB0065-DRVB0517              VALUE 'DRVB0517'.
090201         88 PI-ZWVB0065-DRVB0518              VALUE 'DRVB0518'.
181002         88 PI-ZWVB0065-DRVB0606              VALUE 'DRVB0606'.
230403         88 PI-ZWVB0065-DRVB0657              VALUE 'DRVB0657'.
171203         88 PI-ZWVB0065-DRVB0695              VALUE 'DRVB0695'.
090704         88 PI-ZWVB0065-DRVB0725              VALUE 'DRVB0725'.
210205         88 PI-ZWVB0065-DRVB0755              VALUE 'DRVB0755'.
300807         88 PI-ZWVB0065-DRVB0865              VALUE 'DRVB0865'.
200710         88 PI-ZWVB0065-DRVB0982              VALUE 'DRVB0982'.
030915         88 PI-ZWVB0065-DRVB1118              VALUE 'DRVB1118'.
141015         88 PI-ZWVB0065-DRVB1120              VALUE 'DRVB1120'.
141015         88 PI-ZWVB0065-DRVB1121              VALUE 'DRVB1121'.
141015         88 PI-ZWVB0065-DRVB1122              VALUE 'DRVB1122'.
141015         88 PI-ZWVB0065-DRVB1123              VALUE 'DRVB1123'.
140416         88 PI-ZWVB0065-DRVB1130              VALUE 'DRVB1130'.
210416         88 PI-ZWVB0065-DRVB1131              VALUE 'DRVB1131'.
210416         88 PI-ZWVB0065-DRVB1132              VALUE 'DRVB1132'.
210416         88 PI-ZWVB0065-DRVB1133              VALUE 'DRVB1133'.
290416         88 PI-ZWVB0065-DRVB1136              VALUE 'DRVB1136'.
      ******* ENDE COPY-MEMBER **************************************
 
 

