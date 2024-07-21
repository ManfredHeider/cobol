      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1590.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1590.CPv  $
      *
      *   Rev 2.6   Feb 28 2017 09:55:30   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.5   Apr 07 2016 10:01:50   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.4   Jul 16 2015 15:02:44   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.3   Mar 10 2014 16:49:44   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Feb 28 2014 15:53:22   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Oct 12 2004 08:12:54   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:43:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1590  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01539!28.02.14! 2.2! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!01.04.15! 2.3! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA02220!10.03.16! 2.4! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT_MA_GRD_PR_STZ       !          *
      * APA02275!21.02.17! 2.5! FlexN                     ! Buch     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1590                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMPTH TBPA0317             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1590.
           05 PI-DRPA1590-PMPT-KEY.
              10 PI-DRPA1590-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1590-PMPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1590-PMPT-VERSION
                                       PIC S9(004)       BINARY.
      *
        01 PO-DRPA1590.
           05 PO-DRPA1590-RC           PIC  X(003).
              88 PO-DRPA1590-RC-OK          VALUE 'OK '.
              88 PO-DRPA1590-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1590-PMPT-DATEN.
              10 PO-DRPA1590-PMPT-ID   PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMPT-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMPT-V-GUELTIG-AB
                                       PIC  X(026).
              10 PO-DRPA1590-PMPT-V-GUELTIG-BIS
                                       PIC  X(010).
              10 PO-DRPA1590-PMPT-V-KENNUNG
                                       PIC  X(008).
              10 PO-DRPA1590-PVPT-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1590-PVPT-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMPR-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1590-PMPR-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMBE-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1590-PRINTE    PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1590-PGEB-GES  PIC S9(004)       BINARY.
              10 PO-DRPA1590-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-KNR-PMPT-ART
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMPT-VSTAT
                                       PIC  X(004).
              10 PO-DRPA1590-PMPT-FAELLIG-VON
                                       PIC  X(010).
              10 PO-DRPA1590-PMPT-FAELLIG-BIS
                                       PIC  X(010).
              10 PO-DRPA1590-PMPT-PPFL-BEITRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1590-PMPT-PPFL-SUMME
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1590-PMPT-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1590-PMPT-GRUND-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1590-PMPT-EINH PIC S9(006)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1590-PRKO-KOND-NR-GRP
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PRKO-KOND-NR-EINH
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PRKO-KOND-NR-KDB
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1590-PMPT-LAST-UPDATE
                                       PIC  X(026).
              10 PO-DRPA1590-PMPT-KENNUNG
                                       PIC  X(008).
100316*       10 PO-DRPA1590-PMPT-MA-GRD-PR-STZ
100316*                                PIC S9(3)V9(3) PACKED-DECIMAL.
100316        10 PO-DRPA1590-PMPT-MA-GRD-PR-STZ
100316                                 PIC S9(4)V9(4) PACKED-DECIMAL.
010415        10 PO-DRPA1590-PMPT-PG-UST-BETRAG
010415                                 PIC S9(011)V99 PACKED-DECIMAL.
010415        10 PO-DRPA1590-PMPT-PG-UST-SATZ
010415                                 PIC S9(003)V9(3) PACKED-DECIMAL.
210217        10 PO-DRPA1590-PMPT-VSP-KOMBI-NR
210217                                 PIC S9(005) BINARY.
210217        10 PO-DRPA1590-PMPT-VSP-KOMBI-BZ
210217                                 PIC  X(003).
210217        10 PO-DRPA1590-PMPT-VSP-FN-BER
210217                                 PIC  X(001).
210217        10 PO-DRPA1590-PMPT-VSP-FN-REL
210217                                 PIC  X(001).
210217        10 PO-DRPA1590-PMPT-VSP-FN-FP-KZ
210217                                 PIC  X(001).
 

