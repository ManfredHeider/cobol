      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0215.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRVB0215.CPv  $
      *
      *   Rev 2.10   Feb 04 2010 15:51:10   t13ae27
      *AAS00872: Allfinanz Wettbewerbe 2010 - Progr.
      *
      *   Rev 2.9   Nov 28 2008 10:21:48   t13ae27
      *AAS00793: JWB2009 Begegnungsstätten - Ermittlung
      *
      *   Rev 2.8   Apr 15 2008 15:41:42   t13ae27
      *AAS00747: Wettbewerb 2008 der Allfinanz 
      *
      *   Rev 2.7   13 Dec 2007 16:06:38   t13ae27
      *AAS00725: Jahreswettbewerb 2008/AIDAdiva/Elite-Club Unterneh
      *mensaufbau
      *
      *   Rev 2.6   Feb 27 2006 08:40:04   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.5   Mar 14 2005 09:31:08   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.4   Feb 14 2005 10:52:52   t13ex13
      *AAS00532: Basiszahlen Einheiten neuer VB ab 01.07.2003 je Re
      *gion
      *
      *   Rev 2.3   Feb 01 2005 11:07:08   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.2   Sep 20 2004 10:01:52   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.1   Jun 04 1999 12:51:32   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   15 Dec 1998 10:50:20   T13AE42
      *AAS00079 - Anpassung wg. Delta-Rueckkonvertierung
      *
      *   Rev 1.2   18 Nov 1998 12:14:24   T13AE42
      *AAS00078 - Anpassung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.1   15 May 1998 15:22:28   T13EX30
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:17:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0215                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    05.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!05.03.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      * AAS00078!07.10.98! 02 ! NEUE FUNKTION DRVB0322    ! WALB     *
      * AAS00079!20.11.98! 03 ! NEUE FUNKTION DRVB0326    ! WALB     *
      * AOP00177!31.05.99! 04 ! NEUE FUNKTION DRVB0403    ! SCHMIDT  *
310804* AVD00001!31.08.04!2.2 ! NEUE FUNKTION DRVB0733    ! KLEEMEIER*
011204* AAS00513!01.12.04!2.3 ! NEUE FUNKTION DRVB0748    ! P.Duerr  *
011204* AAS00531!01.02.05!2.4 ! NEUE FUNKTION DRVB0754    ! P.Duerr  *
230106* AAS00595!23.01.06!2.5 ! NEUE FUNKTION DRVB0810    ! P.Duerr  *
061207* AAS00725!06.12.07!2.6 ! NEUE FUNKTION DRVB0880    ! P.Duerr  *
061207* AAS00747!01.04.08!2.7 ! NEUE FUNKTION DRVB0898    ! P.Duerr  *
131108* AAS00793!13.11.08!2.8 ! NEUE FUNKTION DRVB0925    ! P.Duerr  *
291209* AAS00872!29.12.09!2.9 ! NEUE FUNKTION DRVB0963    ! P.Duerr  *
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
       01  PI-ZXVB0012.
           05  PI-ZXVB0012-FUNKTION            PIC  X(008).
               88 PI-ZXVB0012-DRVB0216              VALUE 'DRVB0216'.
071098         88 PI-ZXVB0012-DRVB0322              VALUE 'DRVB0322'.
201198         88 PI-ZXVB0012-DRVB0326              VALUE 'DRVB0326'.
310599         88 PI-ZXVB0012-DRVB0403              VALUE 'DRVB0403'.
310804         88 PI-ZXVB0012-DRVB0733              VALUE 'DRVB0733'.
011204         88 PI-ZXVB0012-DRVB0748              VALUE 'DRVB0748'.
010205         88 PI-ZXVB0012-DRVB0754              VALUE 'DRVB0754'.
230106         88 PI-ZXVB0012-DRVB0810              VALUE 'DRVB0810'.
061207         88 PI-ZXVB0012-DRVB0880              VALUE 'DRVB0880'.
010408         88 PI-ZXVB0012-DRVB0898              VALUE 'DRVB0898'.
131108         88 PI-ZXVB0012-DRVB0925              VALUE 'DRVB0925'.
291209         88 PI-ZXVB0012-DRVB0963              VALUE 'DRVB0963'.
      *
      ******* ENDE COPY-MEMBER DRVB0215 *****************************
      *
