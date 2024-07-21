      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0253.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/DRVB0253.CPv  $
      *
      *   Rev 2.16   Dec 17 2019 10:44:14   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club / AS-Club mbL / Kap.
      *-Club / Adm.-Club - DVAG
      *
      *   Rev 2.15   Oct 30 2019 12:41:30   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.14   Feb 10 2017 15:01:10   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.13   Jan 16 2017 11:02:08   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.12   Oct 05 2009 16:18:02   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.11   Oct 05 2009 16:10:18   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.10   Oct 05 2009 15:50:36   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.9   Oct 05 2009 15:18:08   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.8   Jan 09 2009 17:04:22   t13ae30
      *AAS00779: Allfinanz "Champions-Club"
      *
      *   Rev 2.7   Mar 13 2008 10:51:28   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.6   04 Mar 2008 13:30:30   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.5   Jul 07 2005 11:54:58   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.4   Aug 14 2003 14:10:26   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.3   Dec 17 2002 12:25:36   t13ae30
      *AVB00643 - ENDGÜLTIGE VERSION
      *
      *   Rev 2.2   Nov 15 2002 16:58:18   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.1   Nov 18 1999 15:10:28   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:22:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0253                                      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0014                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *---------------------------------------------------------------*
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    08.11.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00158!08.11.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AVB00643!06.11.02! 02 ! NEUE FUNKTION DRVB00605   ! WILLIG   *
      * AVB00643!09.12.02! 02 ! NEUE FUNKTION DRVB00622   ! WILLIG   *
      * AVB00681!17.07.03! 03 ! NEUE FUNKTION DRVB00665   ! WILLIG   *
      *         !        !    ! NEUE FUNKTION DRVB00666   !          *
      * AVD00009!08.03.05! 04 ! NEUE FUNKTION DRVB00758   ! WILLIG   *
      * AAS00733!18.01.08! 05 ! NEUE FUNKTION DRVB00887   ! P.Duerr  *
      * AAS00746!04.03.08! 06 ! NEUE FUNKTION DRVB00890   ! WILLIG   *
      *         !        !    ! NEUE FUNKTION DRVB00891   !          *
      *         !        !    ! NEUE FUNKTION DRVB00892   !          *
      *         !        !    ! NEUE FUNKTION DRVB00893   !          *
      *         !        !    ! NEUE FUNKTION DRVB00894   !          *
      * AAS00779!01.09.08! 07 ! NEUE FUNKTION DRVB00918   ! WILLIG   *
      * AAS00847!14.08.09! 08 ! NEUE FUNKTION DRVB00950   ! WILLIG   *
      * AAS01186!15.09.16! 09 ! NEUE FUNKTION DRVB01150   ! WILLIG   *
      * VBL-964 !11.09.19! 18 ! WEITERE CLUBS AB 012020   ! WILLIG   *
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
       01  PI-ZJVB0014.
           05  PI-ZJVB0014-FUNKTION            PIC  X(008).
               88 PI-ZJVB0014-DRVB0257              VALUE 'DRVB0257'.
               88 PI-ZJVB0014-DRVB0605              VALUE 'DRVB0605'.
               88 PI-ZJVB0014-DRVB0622              VALUE 'DRVB0622'.
               88 PI-ZJVB0014-DRVB0665              VALUE 'DRVB0665'.
               88 PI-ZJVB0014-DRVB0758              VALUE 'DRVB0758'.
180108         88 PI-ZJVB0014-DRVB0887              VALUE 'DRVB0887'.
040308         88 PI-ZJVB0014-DRVB0890              VALUE 'DRVB0890'.
040308         88 PI-ZJVB0014-DRVB0891              VALUE 'DRVB0891'.
040308         88 PI-ZJVB0014-DRVB0892              VALUE 'DRVB0892'.
040308         88 PI-ZJVB0014-DRVB0893              VALUE 'DRVB0893'.
040308         88 PI-ZJVB0014-DRVB0894              VALUE 'DRVB0894'.
010908         88 PI-ZJVB0014-DRVB0918              VALUE 'DRVB0918'.
140809         88 PI-ZJVB0014-DRVB0950              VALUE 'DRVB0950'.
150916         88 PI-ZJVB0014-DRVB1150              VALUE 'DRVB1150'.
110919         88 PI-ZJVB0014-DRVB1249              VALUE 'DRVB1249'.
110919         88 PI-ZJVB0014-DRVB1250              VALUE 'DRVB1250'.
110919         88 PI-ZJVB0014-DRVB1251              VALUE 'DRVB1251'.
110919         88 PI-ZJVB0014-DRVB1252              VALUE 'DRVB1252'.
110919         88 PI-ZJVB0014-DRVB1253              VALUE 'DRVB1253'.
110919         88 PI-ZJVB0014-DRVB1254              VALUE 'DRVB1254'.
110919         88 PI-ZJVB0014-DRVB1255              VALUE 'DRVB1255'.
110919         88 PI-ZJVB0014-DRVB1256              VALUE 'DRVB1256'.
110919         88 PI-ZJVB0014-DRVB1257              VALUE 'DRVB1257'.
110919         88 PI-ZJVB0014-DRVB1258              VALUE 'DRVB1258'.
110919         88 PI-ZJVB0014-DRVB1259              VALUE 'DRVB1259'.
110919         88 PI-ZJVB0014-DRVB1260              VALUE 'DRVB1260'.
110919         88 PI-ZJVB0014-DRVB1261              VALUE 'DRVB1261'.
110919         88 PI-ZJVB0014-DRVB1262              VALUE 'DRVB1262'.
110919         88 PI-ZJVB0014-DRVB1263              VALUE 'DRVB1263'.
110919         88 PI-ZJVB0014-DRVB1264              VALUE 'DRVB1264'.
110919         88 PI-ZJVB0014-DRVB1265              VALUE 'DRVB1265'.
110919         88 PI-ZJVB0014-DRVB1266              VALUE 'DRVB1266'.
110919         88 PI-ZJVB0014-DRVB1267              VALUE 'DRVB1267'.
110919         88 PI-ZJVB0014-DRVB1275              VALUE 'DRVB1275'.
110919         88 PI-ZJVB0014-DRVB1276              VALUE 'DRVB1276'.
110919         88 PI-ZJVB0014-DRVB1277              VALUE 'DRVB1277'.
      *
      ******* ENDE COPY-MEMBER DRVB0253 *****************************

