      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0717.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0717.CPv  $
      *
      *   Rev 2.2   Feb 03 2016 14:57:44   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.1   Jul 16 2015 15:07:52   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   May 26 1999 15:15:34   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0717  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * Apa00012!15.08.98! 1.1! Buchungsgrund & KZ-Einheit! Parent   *
      *         !        !    !                           !          *
      * apa00012!24.08.98! 02 ! ergänzt um PDPE-Daten (DN)! Parent   *
      * APA01932!20.04.15! 2.1! UMSATZSTEUER EINGEFÜHRT   ! BUCH     *
      * APA02208!29.01.16! 2.2! STEUERJAHR ERGÄNZT        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0717                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0717.
          05 PI-DRPA0717-PDPT-LAST-UPDATE         PIC X(26).
          05 PI-DRPA0717-PDPT-KEY.
             10 PI-DRPA0717-PDPT-ID               PIC S9(15)V COMP-3.
          05 PI-DRPA0717-PDPT-DATEN.
             10 PI-DRPA0717-PDPT-KENNUNG          PIC X(8).
             10 PI-DRPA0717-PMPT-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0717-PMPT-VERSION          PIC S9(4) COMP.
             10 PI-DRPA0717-PEST-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0717-PDPT-BUCH-RFOLGE      PIC S9(4) COMP.
             10 PI-DRPA0717-PDPT-MA-ABR-DAT       PIC X(10).
             10 PI-DRPA0717-KNR-BUCHUNGS-STAND    PIC S9(4) COMP.
             10 PI-DRPA0717-KNR-BUCHUNGSART       PIC S9(4) COMP.
150898       10 PI-DRPA0717-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.
150898       10 PI-DRPA0717-KNR-KZ-EH-MAX         PIC S9(004) BINARY.
             10 PI-DRPA0717-KNR-BERCH-WAEHRUNG    PIC S9(4) COMP.
             10 PI-DRPA0717-PDPT-DGV              PIC S9(4) COMP.
             10 PI-DRPA0717-PDPT-PRODART          PIC S9(4) COMP.
240898       10 PI-DRPA0717-PDPT-PDPE-DATEN.
240898          15 PI-DRPA0717-PDPT-PDPE-DAT-LEN  PIC S9(4) USAGE COMP.
200415*         15 PI-DRPA0717-PDPT-PDPE-DAT-TEXT PIC X(720).
200415          15 PI-DRPA0717-PDPT-PDPE-DAT-TEXT PIC X(1220).
290116       10 PI-DRPA0717-PDPT-UST-JAHR         PIC S9(4) BINARY.
      *
       01 PO-DRPA0717.
          05 PO-DRPA0717-RC                       PIC  X(003).
             88 PO-DRPA0717-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0717-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0717-RC-NOT-FOUND                  VALUE 'NFD'.
 
 

