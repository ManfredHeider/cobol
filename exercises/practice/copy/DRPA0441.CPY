      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0441.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0441.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:06:40   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:44:42   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jun 10 2005 09:48:46   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Feb 27 1999 14:29:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:10:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:37:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0441                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE und PMPP >>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    12.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.08.97!    ! NEU                       ! D.WILHELM*
      * APA01526!15.07.13!2.2 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15!2.3 ! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0441                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0441.
          05 PI-DRPA0441-PROGRAMM             PIC  X(08).
          05 PI-DRPA0441-UFKT                 PIC  9(02).
          05 PI-DRPA0441-SCHALTER             PIC  9.
          05 PI-DRPA0441-DATEN.
             10 PI-DRPA0441-PDTA-ID           PIC  S9(15)V COMP-3.
             10 PI-DRPA0441-PGEB-GES          PIC  S9(04) COMP.
             10 PI-DRPA0441-PMPE-VSTAT        PIC  X(04).
      *
       01 PO-DRPA0441.
          05 PO-DRPA0441-RC                   PIC  X(03).
             88 PO-DRPA0441-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0441-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0441-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0441-DATEN.
             10 PO-DRPA0441-PMPG-ID            PIC S9(15)V COMP-3.
             10 PO-DRPA0441-PMPG-VERSION       PIC S9(04)  COMP.
             10 PO-DRPA0441-PMPE-VNR           PIC X(15).
             10 PO-DRPA0441-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA0441-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA0441-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA0441-PROD-PROD-SCHL     PIC S9(04) COMP.
             10 PO-DRPA0441-PMPP-PG-GSMT-PROV  PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA0441-PMPP-PGV           PIC S9(04) COMP.
150713       10 PO-DRPA0441-PMPE-UNR           PIC S9(04) COMP.
150713       10 PO-DRPA0441-PMPE-SCHICHT-NR    PIC S9(04) COMP.
220715       10 PO-DRPA0441-PMPP-PG-UST-BETRAG PIC S9(09)V9(2) COMP-3.

