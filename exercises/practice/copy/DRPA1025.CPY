      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1025.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1025.CPv  $
      *
      *   Rev 2.2   May 04 2015 10:08:26   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.1   Apr 30 1999 15:35:08   T13EX21
      *APA00012 - Erweiterung
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:21:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:47:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1025                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST PMPP üBER PMPR_ID FüR DIALOG PO21   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B. BOHLINGER                                     *
      *    DATUM:    22.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.07.98!01.01! NEU                      ! BOHLINGER*
      * APA00012!22.04.99!01.02! ERWEITERUNG PO-BEREICH   ! STORCH   *
      * APA01932!14.04.15!01.03! ERWEITERUNG PO-BEREICH   ! KÖPRÜCÜ  *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1025                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1025.
          05 PI-DRPA1025-PROGRAMM              PIC  X(008).
          05 PI-DRPA1025-UFKT                  PIC  9(002).
          05 PI-DRPA1025-DATEN.
             10 PI-DRPA1025-PMPG-ID            PIC  S9(15) COMP-3.
      *
       01 PO-DRPA1025.
          05 PO-DRPA1025-RC                    PIC  X(003).
             88 PO-DRPA1025-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1025-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1025-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1025-DATEN.
             10 PO-DRPA1025-PMPP-ID            PIC  S9(15) COMP-3.
             10 PO-DRPA1025-PMPP-PGV           PIC S9(04) COMP.
             10 PO-DRPA1025-PMPP-PG-GSMT-PROV  PIC S9(09)V9(2) COMP-3.
220499       10 PO-DRPA1025-PROD-PROD-SCHL     PIC S9(04) COMP.
140415       10 PO-DRPA1025-PMPP-PG-UST-BETRAG PIC S9(09)V9(2) COMP-3.
140415       10 PO-DRPA1025-PMPP-PG-UST-SATZ   PIC S9(03)V9(3) COMP-3.

