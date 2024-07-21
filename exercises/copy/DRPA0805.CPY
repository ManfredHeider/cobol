      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0805.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0805.CPv  $
      *
      *   Rev 2.5   Feb 28 2017 09:55:06   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.4   Apr 07 2016 10:01:22   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.3   Jul 16 2015 15:01:30   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Mar 10 2014 16:48:06   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:42:50   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   May 26 1999 13:16:18   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:43:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0805  Funktion Select-all Standard-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA01539!26.07.13!2.1 ! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!01.04.15!2.2 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA02220!10.03.16!2.3 ! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT_MA_GRD_PR_STZ       !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0805                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMPT TBPA0117              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0805.
          05 PI-DRPA0805-PMPT-KEY.
           10 PI-DRPA0805-PMPT-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0805-PMPT-VERSION      PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0805.
          05 PO-DRPA0805-RC                 PIC  X(003).
             88 PO-DRPA0805-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0805-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0805-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0805-PMPT-DATEN.
           10 PO-DRPA0805-PMPT-LAST-UPDATE  PIC X(26).
           10 PO-DRPA0805-PMPT-KENNUNG      PIC X(8).
           10 PO-DRPA0805-PMPT-VERSION      PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PMPT-V-GUELTIG-AB PIC X(26).
           10 PO-DRPA0805-PMPT-V-GUELTIG-BIS PIC X(10).
           10 PO-DRPA0805-PMPT-V-KENNUNG    PIC X(8).
           10 PO-DRPA0805-PVPT-ID           PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0805-PVPT-VERSION      PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PMPR-ID           PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0805-PMPR-VERSION      PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PMBE-ID           PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0805-PRINTE            PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0805-PGEB-GES          PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PROD-PROD-SCHL    PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-KNR-PMPT-ART      PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PMPT-VSTAT        PIC X(4).
           10 PO-DRPA0805-PMPT-FAELLIG-VON  PIC X(10).
           10 PO-DRPA0805-PMPT-FAELLIG-BIS  PIC X(10).
           10 PO-DRPA0805-PMPT-PPFL-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0805-PMPT-PPFL-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0805-PMPT-GSMT-PROV    PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0805-PMPT-GRUND-PROV   PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0805-PMPT-EINH         PIC S9(6)V9(2) USAGE COMP-3.
           10 PO-DRPA0805-PRKO-KOND-NR-GRP  PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PRKO-KOND-NR-EINH PIC S9(4) USAGE COMP.
           10 PO-DRPA0805-PRKO-KOND-NR-KDB  PIC S9(4) USAGE COMP.
100316*    10 PO-DRPA0805-PMPT-MA-GRD-PR-STZ
100316*                                     PIC S9(3)V9(3) USAGE COMP-3.
100316     10 PO-DRPA0805-PMPT-MA-GRD-PR-STZ
100316                                      PIC S9(4)V9(4) USAGE COMP-3.
010415     10 PO-DRPA0805-PMPT-PG-UST-BETRAG  PIC S9(011)V99 COMP-3.
010415     10 PO-DRPA0805-PMPT-PG-UST-SATZ  PIC S9(003)V9(3) COMP-3.
310117     10 PO-DRPA0805-PMPT-VSP-KOMBI-NR PIC S9(005) BINARY.
310117     10 PO-DRPA0805-PMPT-VSP-KOMBI-BZ PIC  X(003).
310117     10 PO-DRPA0805-PMPT-VSP-FN-BER   PIC  X(001).
310117     10 PO-DRPA0805-PMPT-VSP-FN-REL   PIC  X(001).
310117     10 PO-DRPA0805-PMPT-VSP-FN-FP-KZ PIC  X(001).
      *
      ****** Ende Copy-Member ****************************************
 
 

