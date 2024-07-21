      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1638.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1638.CPv  $
      *
      *   Rev 2.6   Feb 28 2017 09:55:36   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.5   Apr 07 2016 10:01:32   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.4   Jul 16 2015 15:02:54   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.3   Mar 10 2014 16:50:14   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jul 26 2013 11:43:46   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Oct 26 2004 12:03:34   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Oct 26 2004 11:22:16   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1638  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      *    VERSION   2.0                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00295!15.07.04!2.0 ! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01539!26.07.13!2.2 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!01.04.15!2.3 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA02220!10.03.16!2.4 ! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT_MA_GRD_PR_STZ       !          *
      * APA02275!07.02.17!2.6 ! Ergänzung FlexN           ! Buch     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1638                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL MIT VERSION<<<   TABELLE PMPT TBPA0117      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1638.
          05 PI-DRPA1638-PMPT-KEY.
           10 PI-DRPA1638-PMPT-ID      PIC S9(15)        PACKED-DECIMAL.
           10 PI-DRPA1638-PMPT-VERSION PIC S9(04)        BINARY.
      *
       01 PO-DRPA1638.
          05 PO-DRPA1638-RC            PIC  X(003).
             88 PO-DRPA1638-RC-OK           VALUE 'OK '.
             88 PO-DRPA1638-RC-CHANGED      VALUE 'CHA'.
             88 PO-DRPA1638-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRPA1638-PMPT-DATEN.
           10 PO-DRPA1638-PMPT-LAST-UPDATE   PIC  X(026).
           10 PO-DRPA1638-PMPT-KENNUNG       PIC  X(008).
           10 PO-DRPA1638-PMPT-VERSION       PIC S9(04) BINARY.
           10 PO-DRPA1638-PMPT-V-GUELTIG-AB  PIC  X(026).
           10 PO-DRPA1638-PMPT-V-GUELTIG-BIS PIC  X(010).
           10 PO-DRPA1638-PMPT-V-KENNUNG     PIC  X(008).
           10 PO-DRPA1638-PVPT-ID            PIC S9(15) PACKED-DECIMAL.
           10 PO-DRPA1638-PVPT-VERSION       PIC S9(04) BINARY.
           10 PO-DRPA1638-PMPR-ID            PIC S9(15) PACKED-DECIMAL.
           10 PO-DRPA1638-PMPR-VERSION       PIC S9(04) BINARY.
           10 PO-DRPA1638-PMBE-ID            PIC S9(15) PACKED-DECIMAL.
           10 PO-DRPA1638-PRINTE             PIC S9(15) PACKED-DECIMAL.
           10 PO-DRPA1638-PGEB-GES           PIC S9(04) BINARY.
           10 PO-DRPA1638-PROD-PROD-SCHL     PIC S9(04) BINARY.
           10 PO-DRPA1638-KNR-PMPT-ART       PIC S9(04) BINARY.
           10 PO-DRPA1638-PMPT-VSTAT         PIC  X(004).
           10 PO-DRPA1638-PMPT-FAELLIG-VON   PIC  X(010).
           10 PO-DRPA1638-PMPT-FAELLIG-BIS   PIC  X(010).
           10 PO-DRPA1638-PMPT-PPFL-BEITRAG
                                       PIC S9(09)V9(2)   PACKED-DECIMAL.
           10 PO-DRPA1638-PMPT-PPFL-SUMME
                                       PIC S9(09)V9(2)   PACKED-DECIMAL.
           10 PO-DRPA1638-PMPT-GSMT-PROV
                                       PIC S9(09)V9(2)   PACKED-DECIMAL.
           10 PO-DRPA1638-PMPT-GRUND-PROV
                                       PIC S9(09)V9(2)   PACKED-DECIMAL.
           10 PO-DRPA1638-PMPT-EINH
                                       PIC S9(06)V9(2)   PACKED-DECIMAL.
           10 PO-DRPA1638-PRKO-KOND-NR-GRP
                                       PIC S9(04)        BINARY.
           10 PO-DRPA1638-PRKO-KOND-NR-EINH
                                       PIC S9(04)        BINARY.
           10 PO-DRPA1638-PRKO-KOND-NR-KDB
                                       PIC S9(04)        BINARY.
100316*    10 PO-DRPA1638-PMPT-MA-GRD-PR-STZ
100316*                                PIC S9(3)V9(3)    PACKED-DECIMAL.
100316     10 PO-DRPA1638-PMPT-MA-GRD-PR-STZ
100316                                 PIC S9(4)V9(4)    PACKED-DECIMAL.
010415     10 PO-DRPA1638-PMPT-PG-UST-BETRAG  PIC S9(011)V99 COMP-3.
010415     10 PO-DRPA1638-PMPT-PG-UST-SATZ    PIC S9(003)V9(3) COMP-3.
070217     10 PO-DRPA1638-PMPT-VSP-KOMBI-NR   PIC S9(005) BINARY.
070217     10 PO-DRPA1638-PMPT-VSP-KOMBI-BZ   PIC  X(003).
070217     10 PO-DRPA1638-PMPT-VSP-FN-BER     PIC  X(001).
070217     10 PO-DRPA1638-PMPT-VSP-FN-REL     PIC  X(001).
070217     10 PO-DRPA1638-PMPT-VSP-FN-FP-KZ   PIC  X(001).
      *
 
 

