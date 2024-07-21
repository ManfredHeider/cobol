      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0448.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0448.CPv  $
      *
      *   Rev 2.4   Jul 07 2005 11:55:36   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.3   Aug 14 2003 14:10:58   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.2   Nov 15 2002 16:58:52   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.1   Nov 18 1999 15:11:44   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Aug 04 1999 10:30:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0448                                      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0014                        *
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
      * AVB00643!06.11.02! 02 ! NEUE FUNKTION DRVB00607   ! WILLIG   *
      * AVB00681!30.07.03! 03 ! NEUE FUNKTION DRVB00667   ! WILLIG   *
      * AVD00009!08.03.05! 04 ! NEUE FUNKTION DRVB00760   ! WILLIG   *
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
       01  PI-UPVB0111.
           05  PI-UPVB0111-FUNKTION            PIC  X(008).
               88 PI-UPVB0111-DRVB0449              VALUE 'DRVB0449'.
               88 PI-UPVB0111-DRVB0607              VALUE 'DRVB0607'.
               88 PI-UPVB0111-DRVB0667              VALUE 'DRVB0667'.
               88 PI-UPVB0111-DRVB0760              VALUE 'DRVB0760'.
      *
      ******* ENDE COPY-MEMBER DRVB0448 *****************************
