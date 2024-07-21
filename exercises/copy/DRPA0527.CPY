      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0527.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0527.CPv  $
      *
      *   Rev 2.2   Jul 30 2015 09:06:30   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.1   Dec 17 2013 10:45:14   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.0   Feb 27 1999 14:29:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:29:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 17:00:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0527                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE und PMPP >>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: E.SCHRIEBER                                      *
      *    DATUM:    25.09.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!25.09.97!    ! NEU                       !SCHRIEBER *
      * APA01526!15.07.13!2.1 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15!2.2 ! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0527                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0527.
          05 PI-DRPA0527-PROGRAMM             PIC  X(008).
          05 PI-DRPA0527-UFKT                 PIC  9(002).
          05 PI-DRPA0527-SCHALTER             PIC  9.
          05 PI-DRPA0527-DATEN.
             10 PI-DRPA0527-PMPG-ID           PIC S9(15)V COMP-3.
             10 PI-DRPA0527-PMPG-VERSION      PIC S9(04) COMP.
      *
       01 PO-DRPA0527.
          05 PO-DRPA0527-RC                   PIC  X(003).
             88 PO-DRPA0527-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0527-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0527-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0527-DATEN.
             10 PO-DRPA0527-PMPE-VNR           PIC X(15).
             10 PO-DRPA0527-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA0527-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA0527-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA0527-PROD-PROD-SCHL     PIC S9(4)  COMP.
             10 PO-DRPA0527-PMPP-PG-GSMT-PROV  PIC S9(9)V9(2) COMP-3.
150713       10 PO-DRPA0527-PMPE-UNR           PIC S9(04) COMP.
150713       10 PO-DRPA0527-PMPE-SCHICHT-NR    PIC S9(04) COMP.
220715       10 PO-DRPA0527-PMPP-PG-UST-BETRAG PIC S9(9)V9(2) COMP-3.


