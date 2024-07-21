      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0802.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0802.CPv  $
      *
      *   Rev 2.6   Feb 28 2017 09:54:52   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.5   Apr 07 2016 10:00:18   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.4   Jul 16 2015 15:01:08   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.3   Mar 10 2014 16:47:50   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jul 26 2013 11:42:40   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Nov 02 2006 14:56:20   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 13:16:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:43:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0802  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.01.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA01539!26.07.13! 2.2! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!01.04.15! 2.3! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA02220!09.03.16! 2.4! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT_MA_GRD_PR_STZ       !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0802                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMPT TBPA0117              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0802.
          05 PI-DRPA0802-PROGRAMM           PIC  X(008).
          05 PI-DRPA0802-UFKT               PIC  9(002).
          05 PI-DRPA0802-PMPT-DATEN.
           10 PI-DRPA0802-PMPT-ID           PIC S9(15)V COMP-3.
           10 PI-DRPA0802-PMPT-VERSION      PIC S9(4)   COMP.
           10 PI-DRPA0802-PVPT-ID           PIC S9(15)V COMP-3.
           10 PI-DRPA0802-PVPT-VERSION      PIC S9(4)   COMP.
           10 PI-DRPA0802-PMPR-ID           PIC S9(15)V COMP-3.
           10 PI-DRPA0802-PMPR-VERSION      PIC S9(4)   COMP.
           10 PI-DRPA0802-PMBE-ID           PIC S9(15)V COMP-3.
           10 PI-DRPA0802-PRINTE            PIC S9(15)V COMP-3.
           10 PI-DRPA0802-PGEB-GES          PIC S9(4)   COMP.
           10 PI-DRPA0802-PROD-PROD-SCHL    PIC S9(4) BINARY.
           10 PI-DRPA0802-KNR-PMPT-ART      PIC S9(4)   COMP.
           10 PI-DRPA0802-PMPT-VSTAT        PIC X(4).
           10 PI-DRPA0802-PMPT-FAELLIG-VON  PIC X(10).
           10 PI-DRPA0802-PMPT-FAELLIG-BIS  PIC X(10).
           10 PI-DRPA0802-PMPT-PPFL-BEITRAG PIC S9(9)V9(2) COMP-3.
           10 PI-DRPA0802-PMPT-PPFL-SUMME   PIC S9(9)V9(2) COMP-3.
           10 PI-DRPA0802-PMPT-GSMT-PROV    PIC S9(9)V9(2) COMP-3.
           10 PI-DRPA0802-PMPT-GRUND-PROV   PIC S9(9)V9(2) COMP-3.
           10 PI-DRPA0802-PMPT-EINH         PIC S9(6)V9(2) COMP-3.
           10 PI-DRPA0802-PRKO-KOND-NR-GRP  PIC S9(4) COMP.
           10 PI-DRPA0802-PRKO-KOND-NR-EINH PIC S9(4) COMP.
           10 PI-DRPA0802-PRKO-KOND-NR-KDB  PIC S9(4) COMP.
090316*    10 PI-DRPA0802-PMPT-MA-GRD-PR-STZ
090316*                                     PIC S9(3)V9(3) COMP-3.
090316     10 PI-DRPA0802-PMPT-MA-GRD-PR-STZ
090316                                      PIC S9(4)V9(4) COMP-3.
010415     10 PI-DRPA0802-PMPT-PG-UST-BETRAG  PIC S9(011)V99 COMP-3.
010415     10 PI-DRPA0802-PMPT-PG-UST-SATZ  PIC S9(003)V9(3) COMP-3.
090316*                                     PIC S9(3)V9(3) COMP-3.
310117     10 PI-DRPA0802-PMPT-VSP-KOMBI-NR PIC S9(005) BINARY.
310117     10 PI-DRPA0802-PMPT-VSP-KOMBI-BZ PIC  X(003).
310117     10 PI-DRPA0802-PMPT-VSP-FN-BER   PIC  X(001).
310117     10 PI-DRPA0802-PMPT-VSP-FN-REL   PIC  X(001).
310117     10 PI-DRPA0802-PMPT-VSP-FN-FP-KZ PIC  X(001).
      *
       01 PO-DRPA0802.
          05 PO-DRPA0802-RC                   PIC  X(003).
             88 PO-DRPA0802-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0802-RC-DUPLICATE-VALUE            VALUE 'DVA'.
 
 
 

