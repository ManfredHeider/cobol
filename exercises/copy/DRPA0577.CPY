      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0577.CPY  $
      *    $Revision:   1.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0577.CPv  $
      *
      *   Rev 1.6   Feb 28 2017 09:54:40   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 1.5   Apr 07 2016 10:00:08   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 1.4   Jul 16 2015 15:00:58   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 1.3   Mar 10 2014 16:45:42   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 1.2   Jul 26 2013 11:41:00   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 1.1   Jan 18 2000 12:02:24   T13AE18
      *APA00051 'Umbuchen einer PMPT'
      *
      *   Rev 1.0   22 Oct 1997 13:12:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0577  FUNKTION INSERT MIT VERSIONSFÜHRUNG*
      *                           STANDARD-OZP ZTPA0117              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    22.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00051!22.09.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA01539!26.07.13!1.3 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!28.04.15!1.4 ! ERWEITERUNG FÜR DVM (LUCY)! STORCH   *
      * APA02220!09.03.16!1.5 ! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    !/PMPT-MA-GRD-PR-STZ        !          *
      * APA02275!07.02.17!1.6 ! Ergänzung FlexN           ! Buch     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0577                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT MIT VERSIONSFÜHRUNG <<< TABELLE PMPT TBPA0117 *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0577.
      ***  LAST-UPDATE DER AKTUELLEN PMPT
          05 PI-DRPA0577-LAST-UPDATE        PIC  X(026).
          05 PI-DRPA0577-PMPT-DATEN.
           10 PI-DRPA0577-PMPT-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0577-PMPT-VERSION      PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PVPT-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0577-PVPT-VERSION      PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PMPR-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0577-PMPR-VERSION      PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PMBE-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0577-PRINTE            PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0577-PGEB-GES          PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PROD-PROD-SCHL    PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-KNR-PMPT-ART      PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PMPT-VSTAT        PIC X(4).
           10 PI-DRPA0577-PMPT-FAELLIG-VON  PIC X(10).
           10 PI-DRPA0577-PMPT-FAELLIG-BIS  PIC X(10).
           10 PI-DRPA0577-PMPT-PPFL-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0577-PMPT-PPFL-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0577-PMPT-GSMT-PROV    PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0577-PMPT-GRUND-PROV   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0577-PMPT-EINH         PIC S9(6)V9(2) USAGE COMP-3.
           10 PI-DRPA0577-PRKO-KOND-NR-GRP  PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PRKO-KOND-NR-EINH PIC S9(4) USAGE COMP.
           10 PI-DRPA0577-PRKO-KOND-NR-KDB  PIC S9(4) USAGE COMP.
090316*    10 PI-DRPA0577-PMPT-MA-GRD-PR-STZ
090316*                                     PIC S9(3)V9(3) USAGE COMP-3.
090316     10 PI-DRPA0577-PMPT-MA-GRD-PR-STZ
090316                                      PIC S9(4)V9(4) USAGE COMP-3.
280415     10 PI-DRPA0577-PMPT-PG-UST-BETRAG
280415                                      PIC S9(9)V9(2) USAGE COMP-3.
280415     10 PI-DRPA0577-PMPT-PG-UST-SATZ  PIC S9(3)V9(3) USAGE COMP-3.
070217     10 PI-DRPA0577-PMPT-VSP-KOMBI-NR PIC S9(005) BINARY.
070217     10 PI-DRPA0577-PMPT-VSP-KOMBI-BZ PIC  X(003).
070217     10 PI-DRPA0577-PMPT-VSP-FN-BER   PIC  X(001).
070217     10 PI-DRPA0577-PMPT-VSP-FN-REL   PIC  X(001).
070217     10 PI-DRPA0577-PMPT-VSP-FN-FP-KZ PIC  X(001).
      *
       01 PO-DRPA0577.
          05 PO-DRPA0577-RC                   PIC  X(003).
             88 PO-DRPA0577-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0577-RC-DUPLICATE-VALUE            VALUE 'DVA'.
             88 PO-DRPA0577-RC-CHANGED                    VALUE 'CHA'.
 
 
 

