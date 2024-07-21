      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0081.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0081.CPv  $
      *
      *   Rev 2.8   Dec 18 2015 08:57:50   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.7   Oct 21 2015 14:39:50   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.6   Oct 24 2013 11:01:12   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.5   23 Nov 2007 11:01:56   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.4   01 Sep 2006 09:47:46   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.3   Dec 30 2003 08:54:36   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.2   Oct 31 2003 10:52:22   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:05:58   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:16:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0081                                     *
      *    FUNKTION   : ERM. ADVC-BESTAND ÜBER GES, PIN              *
      *                 -> AUFBEREITET FÜR AN50 (BAN0021)            *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0081                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0081.
           05 PI-DRAN0081-ADVC-KEY.
               10 PI-DRAN0081-PGEB-GES         PIC  X(2).
               10 PI-DRAN0081-ADVC-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0081-ADVC-VDABVERM    PIC  S9(9) COMP.
      *
       01 PO-DRAN0081.
          05 PO-DRAN0081-RC                      PIC  X(003).
             88 PO-DRAN0081-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0081-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0081-DATEN.
      *  OUTPUT DATA FOR MAP BAN0041
             10 PO-DRAN0081-VBNR81O              PIC X(7).
             10 PO-DRAN0081-PRZVB81O             PIC X(1).
             10 PO-DRAN0081-ZNAME81O             PIC X(26).
             10 PO-DRAN0081-VNAME81O             PIC X(26).
             10 PO-DRAN0081-PLZ81O               PIC X(5).
             10 PO-DRAN0081-ORT81O               PIC X(21).
             10 PO-DRAN0081-AUS81O               PIC X(3).
             10 PO-DRAN0081-PRODKEY81O           PIC 9(9).
             10 PO-DRAN0081-BEGIN81O             PIC X(6).
             10 PO-DRAN0081-ABLAUF81O            PIC X(6).
             10 PO-DRAN0081-ZAHLW81O             PIC X(1).
             10 PO-DRAN0081-BEITRAGBRUTTO81O     PIC X(8).
             10 PO-DRAN0081-VDABLEIN81O          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0081-HAND81O              PIC X(1).
             10 PO-DRAN0081-ANART8O              PIC X(1).
             10 PO-DRAN0081-ANDAT8O              PIC X(8).
             10 PO-DRAN0081-WKZ8O                PIC X(1).
             10 PO-DRAN0081-WKZT8O               PIC X(3).

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0083.

      ******** ENDE COPY-MEMBER **************************************



