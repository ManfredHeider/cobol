      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0446.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRVB0446.CPv  $
      *
      *   Rev 2.10   Dec 17 2019 10:45:48   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club / AS-Club mbL / Kap.
      *-Club / Adm.-Club - DVAG
      *
      *   Rev 2.9   Nov 05 2019 12:53:08   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.8   Oct 30 2019 12:42:16   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.7   Feb 10 2017 15:01:24   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.6   Jan 16 2017 11:02:30   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.5   Nov 25 2009 15:14:50   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.4   Jan 09 2009 17:04:14   t13ae30
      *AAS00779: Allfinanz "Champions-Club"
      *
      *   Rev 2.3   Jul 07 2005 11:58:08   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.2   Nov 15 2002 16:58:50   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.1   Nov 18 1999 15:11:40   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Aug 04 1999 10:30:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0446                                      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *---------------------------------------------------------------*
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    14.10.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00158!14.10.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AVB00643!06.11.02! 02 ! NEUE FUNKTION DRVB00604   ! WILLIG   *
      * AVD00009!02.06.05!2.3 ! NEUE FUNKTION DRVB0780    ! TH.WILLIG*
      * AAS00779!01.09.08!2.4 ! NEUE FUNK.DRVB0919/920    ! TH.WILLIG*
      * AAS00847!14.08.09!2.5 ! NEUE FUNK.DRVB0949        ! TH.WILLIG*
      * AAS01186!15.09.16!2.6 ! ERW. DRVB1156             ! WILLIG   *
      * VBL-964 !11.09.19!2.7 ! NEUE BEDINGUNGEN AB 012020! WILLIG   *
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
       01  PI-ZJVB0013.
           05  PI-ZJVB0013-FUNKTION            PIC  X(008).
               88 PI-ZJVB0013-DRVB0447              VALUE 'DRVB0447'.
               88 PI-ZJVB0013-DRVB0604              VALUE 'DRVB0604'.
               88 PI-ZJVB0013-DRVB0666              VALUE 'DRVB0666'.
               88 PI-ZJVB0013-DRVB0780              VALUE 'DRVB0780'.
               88 PI-ZJVB0013-DRVB0919              VALUE 'DRVB0919'.
               88 PI-ZJVB0013-DRVB0920              VALUE 'DRVB0920'.
               88 PI-ZJVB0013-DRVB0949              VALUE 'DRVB0949'.
               88 PI-ZJVB0013-DRVB1156              VALUE 'DRVB1156'.
               88 PI-ZJVB0013-DRVB1241              VALUE 'DRVB1241'.
               88 PI-ZJVB0013-DRVB1242              VALUE 'DRVB1242'.
               88 PI-ZJVB0013-DRVB1243              VALUE 'DRVB1243'.
               88 PI-ZJVB0013-DRVB1244              VALUE 'DRVB1244'.
               88 PI-ZJVB0013-DRVB1245              VALUE 'DRVB1245'.
               88 PI-ZJVB0013-DRVB1246              VALUE 'DRVB1246'.
               88 PI-ZJVB0013-DRVB1247              VALUE 'DRVB1247'.
               88 PI-ZJVB0013-DRVB1248              VALUE 'DRVB1248'.
      *
      ******* ENDE COPY-MEMBER DRVB0446 *****************************





