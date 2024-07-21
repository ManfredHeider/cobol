      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0372.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/MF/CPY/DRAT0372.CPv  $
      *
      *   Rev 2.7   Mar 03 2021 10:26:06   t13ex67
      *DAISY-3398 Fällig-bis in MA-SST
      *
      *   Rev 2.6   Mar 01 2016 14:49:14   t13ex67
      *APA02208: Anpassungen Korrekturprogramm USt - MSS
      *
      *   Rev 2.5   Aug 12 2015 12:23:58   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: MSC
      *
      *   Rev 2.4   Dec 08 2011 10:51:52   t13ex44
      *APA01304: "KÜP auf sich selbst"
      *
      *   Rev 2.3   May 26 2010 07:11:26   t13ex44
      *APA00993: Kundenübernahme-Folgeprovision (KÜF)
      *
      *   Rev 2.2   Aug 25 2008 10:18:36   t13ex44
      *APA00774: Kundenübernahmeprovision
      *
      *   Rev 2.1   17 May 2001 13:28:54   t13ex43
      *APA00132 : Check Batchbearbeitungsfunktion
      *
      *   Rev 2.0   19 Apr 2001 14:26:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0372  KONSTANTEN FÜR KTY = 197           *
      *                           (CHECK BATCHBEARBEITUNGSFUNKTION)  *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    15.05.2001                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00132!15.05.01! 2.01! NEU                       !KLEEMEIER*
      * APA00774!26.06.08! 2.02! KUNDENUEBERNAHMEPROVISION ! SCHEIDEL*
      * APA00993!29.04.10! 2.03! KUNDENUEBERNAHME-FOLGE    ! SCHEIDEL*
      * APA01304!17.11.11! 2.4 ! KUEP-QUEREINSTEIGER       ! SCHEIDEL*
      * APA01932!10.08.15! 2.5 ! NEUTRALISIERUNG VIA KONFIG! SCHEIDEL*
      * APA02208!08.02.16! 2.6 ! UMBUCHUNG VIA KONFIG.     ! STORCH  *
      * DSY-3398!02.03.21! 2.7 ! NEU: FÄLLIG-BIS AN SST    ! STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-BAT-BEFU-ZUK-FAELLIG  PIC S9(004) BINARY VALUE 1.
           05 K-KNR-BAT-BEFU-EH-MAX       PIC S9(004) BINARY VALUE 2.
260608     05 K-KNR-BAT-BEFU-KDUE         PIC S9(004) BINARY VALUE 3.
290410     05 K-KNR-BAT-BEFU-KUEF         PIC S9(004) BINARY VALUE 4.
171111     05 K-KNR-BAT-BEFU-QUER         PIC S9(004) BINARY VALUE 5.
100815     05 K-KNR-BAT-NEUTR-ZULAESSIG   PIC S9(004) BINARY VALUE 6.
080216     05 K-KNR-BAT-UMBUCH-ZULAESSIG  PIC S9(004) BINARY VALUE 7.
020321     05 K-KNR-BAT-FAELLIG-AN-SST    PIC S9(004) BINARY VALUE 8.
      *
      ******** ENDE COPY-MEMBER **************************************


