      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0080.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0080.CPv  $
      *
      *   Rev 2.8   Dec 18 2015 08:57:42   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.7   Oct 21 2015 14:39:40   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.6   Oct 24 2013 11:01:08   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.5   23 Nov 2007 11:01:48   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.4   01 Sep 2006 09:47:34   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.3   Dec 30 2003 08:53:46   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.2   Oct 31 2003 10:51:54   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:05:50   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:15:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0080                                     *
      *    FUNKTION   : ERM. ADVC-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR ADVOCARD                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1  !NEU                        !RAHNER   *
      * AAN00422!31.10.03!2.2  !VERGRÖßERN BLACKBOX        !RAHNER   *
      * AAN00422!30.12.03!2.3  !AKTIVIEREN ÄNDERUNGEN VOM  !         *
      *         !        !     !31.10.2003                 !RAHNER   *
      * AAN00730!30.11.07!2.5  !ALLFINANZ                  !RAHNER   *
      * AAN01073!24.10.13!2.6  !SEPA                       !RAHNER   *
      * AAN01223!08.12.15!2.8  !BEGINN/ABLAUF ÄNDERBAR     !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0080                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0080.
           05 PI-DRAN0080-ADVC-KEY.
               10 PI-DRAN0080-PGEB-GES           PIC X(2).
               10 PI-DRAN0080-ADVC-HVANTNUM      PIC X(9).
      *           OPTIONAL
               10 PI-DRAN0080-ADVC-VDABVERM      PIC  X(7).
               10 PI-DRAN0080-ADVC-VDABVERMPZ    PIC  X(1).
      *
       01 PO-DRAN0080.
          05 PO-DRAN0080-RC                      PIC   X(003).
             88 PO-DRAN0080-RC-OK                VALUE 'OK '.
             88 PO-DRAN0080-RC-NOK               VALUE 'NOK'.
             88 PO-DRAN0080-RC-KEY-NOT-UNIQUE    VALUE 'KNU'.
          05 PO-DRAN0080-DATEN.
      *  OUTPUT DATA FOR MAP BAN0021
             10 PO-DRAN0080-VBNR81O              PIC X(7).
             10 PO-DRAN0080-PRZVB81O             PIC X(1).
             10 PO-DRAN0080-ZNAME81O             PIC X(26).
             10 PO-DRAN0080-VNAME81O             PIC X(26).
             10 PO-DRAN0080-PLZ81O               PIC X(5).
             10 PO-DRAN0080-ORT81O               PIC X(21).
             10 PO-DRAN0080-AUS81O               PIC X(3).
             10 PO-DRAN0080-PRODKEY81O           PIC 9(4).
             10 PO-DRAN0080-BEGIN81O             PIC X(6).
             10 PO-DRAN0080-ABLAUF81O            PIC X(6).
             10 PO-DRAN0080-ZAHLW81O             PIC X(1).
             10 PO-DRAN0080-BEITRAGBRUTTO81O     PIC X(8).
             10 PO-DRAN0080-VDABLEIN81O          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0080-HAND81O              PIC X(1).
             10 PO-DRAN0080-ANART81O             PIC X(1).
             10 PO-DRAN0080-ANDAT81O             PIC X(8).
             10 PO-DRAN0080-WKZ81O               PIC X(1).
             10 PO-DRAN0080-WKZT81O              PIC X(3).
             10 PO-DRAN0080-SONSTIGE.
                15 PO-DRAN0080-PIN81O            PIC S9(9) COMP.
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0083.
      *
      ******** ENDE COPY-MEMBER **************************************



