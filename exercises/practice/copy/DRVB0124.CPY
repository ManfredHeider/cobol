      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0124.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRVB0124.CPv  $
      *
      *   Rev 2.13   Jan 07 2021 11:48:28   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.12   Jul 08 2015 12:33:12   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VB22
      *
      *   Rev 2.11   Mar 10 2008 08:10:42   t13ae13
      *AAS00744: Siegeradler 2008
      *
      *   Rev 2.10   17 Jul 2007 08:06:48   t13ae13
      *AVB00923: neue Aufstiegsbedingungen DVAG - XA02
      *
      *   Rev 2.9   Sep 20 2004 10:01:24   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.8   Sep 17 2004 11:15:20   t13ex22
      *AAS00425: Vergleich Beförderungsbed. RGS - RD2
      *
      *   Rev 2.7   Mar 24 2003 15:13:26   t13ex22
      *FVB00521: Neue Funktion DRVB0647
      *
      *   Rev 2.6   Jan 14 2003 09:24:00   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.5   Jun 20 2002 14:23:56   t13ae13
      *AVB00606: Einführung Seminare für DVAG-Wien
      *
      *   Rev 2.4   Apr 30 2002 09:08:44   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.3   05 Dec 2001 09:04:40   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.2   13 Oct 2000 09:31:54   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.1   Jun 04 1999 12:52:30   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   20 Nov 1998 12:17:36   T13AE42
      *ASL00024 - Anpassung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.2   26 Aug 1998 10:23:40   T13AE35
      *AAS00049 - Aenderung, neue Fkt DRVB0276
      *
      *   Rev 1.1   Mar 06 1997 15:13:30   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    22.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!22.01.97! 01 ! NEU                       ! SCHÄFER  *
      * AAS00049!20.06.98! 02 ! NEUE FUNKTION DRVB0276    ! WALB     *
      * ASL00024!23.10.98! 03 ! NEUE FUNKTION DRVB0329    ! SCHMIDT,B*
      * AOP00177!31.05.99! 04 ! NEUE FUNKTION DRVB0375    ! SCHMIDT,B*
      * AVB00490!28.06.00! 05 ! NEUE FUNKTIONEN DRVB0493, ! Walb     *
      *         !08.08.00!    ! DRVB0498                  !          *
      * AVB00544!12.11.01! 06 ! NEUE FUNKTION DRVB0565    ! Walb     *
      * ACV00007!29.01.02! 07 ! NEUE FUNKTION DRVB0566    ! Dürr     *
      * AVB00606!12.06.02! 08 ! NEUE FUNKTION DRVB0586    ! Walb     *
      * AVB00649!03.12.02!2.6 ! NEUE FUNKTION DRVB0381    ! WALB     *
      * FVB00521!24.03.03!2.7 ! NEUE FUNKTION DRVB0647    ! MUND     *
      * AVB00669!19.08.03!2.8 ! NEUE FUNKTION DRVB0669    ! BOLL     *
200904* AVD00001!20.09.04!2.9 ! NEUE FUNKTION DRVB0739    ! KLEEMEIER*
      * AVB00923!02.07.07!2.10! NEUE FUNKTION DRVB0859    ! WALB     *
      * AAS00744!29.02.08!2.11! NEUE FUNKTION DRVB0900    ! WALB     *
      * AVB01332!16.06.15!2.12! NEUE FUNKTION DRVB1107    ! WALB     *
201120* VLA-532 !20.11.20!2.13! NEUE FUNKTION DRVB1296    ! KLEEMEIER*
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
       01  PI-ZWVB0009.
           05  PI-ZWVB0009-FUNKTION            PIC  X(008).
               88 PI-ZWVB0009-DRVB0125              VALUE 'DRVB0125'.
200698         88 PI-ZWVB0009-DRVB0276              VALUE 'DRVB0276'.
231098         88 PI-ZWVB0009-DRVB0329              VALUE 'DRVB0329'.
310599         88 PI-ZWVB0009-DRVB0375              VALUE 'DRVB0375'.
280600         88 PI-ZWVB0009-DRVB0493              VALUE 'DRVB0493'.
080800         88 PI-ZWVB0009-DRVB0498              VALUE 'DRVB0498'.
121101         88 PI-ZWVB0009-DRVB0565              VALUE 'DRVB0565'.
290102         88 PI-ZWVB0009-DRVB0566              VALUE 'DRVB0566'.
120602         88 PI-ZWVB0009-DRVB0586              VALUE 'DRVB0586'.
031202         88 PI-ZWVB0009-DRVB0381              VALUE 'DRVB0381'.
240303         88 PI-ZWVB0009-DRVB0647              VALUE 'DRVB0647'.
190803         88 PI-ZWVB0009-DRVB0669              VALUE 'DRVB0669'.
200904         88 PI-ZWVB0009-DRVB0739              VALUE 'DRVB0739'.
020707         88 PI-ZWVB0009-DRVB0859              VALUE 'DRVB0859'.
290208         88 PI-ZWVB0009-DRVB0900              VALUE 'DRVB0900'.
160615         88 PI-ZWVB0009-DRVB1107              VALUE 'DRVB1107'.
201120         88 PI-ZWVB0009-DRVB1296              VALUE 'DRVB1296'.
      *
      ******* ENDE COPY-MEMBER DRVB0124 *****************************
      *


