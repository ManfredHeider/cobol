      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0632.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0632.CPv  $
      *
      *   Rev 2.2   Jul 29 2015 10:12:34   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System,
      *DTA T
      *rennen
      *
      *   Rev 2.1   11 Sep 2000 08:40:42   T13EX21
      *APA00098 - Erweiterung PO-Bereich
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:19:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:56:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0632                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF S-FEHLER ERMITTELN                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    19.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!19.11.97! 01 ! NEU                       ! D.WILHELM*
      * APA00098!07.09.00!02.1! ERWEITERUNG UM PO-SCHALTER! STORCH   *
      *         !        !    ! SPERRKRITERIEN-VORHANDEN  !          *
      * APA01932!29.07.15!02.2! ERWEITERUNG UM PO-FELD    ! KÖPRÜCÜ  *
      *         !        !    ! SUMME-UST-BETRAG (LUCY)   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0632                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0632.
          05 PI-DRPA0632-PROGRAMM            PIC  X(08).
          05 PI-DRPA0632-UFKT                PIC  9(02).
          05 PI-DRPA0632-DATEN.
             10 PI-DRPA0632-DTA-ID           PIC S9(15) USAGE COMP-3.
             10 PI-DRPA0632-VSTAT-KLASSE     PIC  X(01).
             10 PI-DRPA0632-PGEB-GES         PIC S9(04) USAGE COMP.
             10 PI-DRPA0632-KNR-DTA-ART      PIC S9(04) USAGE COMP.
      *
       01 PO-DRPA0632.
          05 PO-DRPA0632-RC                  PIC  X(03).
             88 PO-DRPA0632-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0632-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0632-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0632-DATEN.
             10 PO-DRPA0632-VSTAT                PIC  X(04).
             10 PO-DRPA0632-ANZAHL-BEWEGUNGEN    PIC S9(09) USAGE COMP.
             10 PO-DRPA0632-SUMME-GUTSCHRIFTEN   PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PO-DRPA0632-SUMME-BELASTUNGEN    PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
070900       10 PO-DRPA0632-S-SPERRKRIT          PIC  X(01).
      ***       (1: SPERRKRITERIEN VORHANDEN; 2: NICHT VORHANDEN)
290715       10 PO-DRPA0632-SUMME-UST-BETRAG     PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
      ******* ENDE COPY-MEMBER *******

