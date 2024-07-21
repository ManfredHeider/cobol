      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0719.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0719.CPv  $
      *
      *   Rev 2.2   Feb 03 2016 14:57:52   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.1   Jul 16 2015 15:08:00   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   May 26 1999 15:15:36   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0719  Funktion Select Standard-OZP       *
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
      *         !        !    !                           !          *
      * apa00012!24.08.98! 02 ! ergänzt um PDPE-Daten (DN)! Parent   *
      * APA01932!20.04.15! 2.1! UMSATZSTEUER ERGÄNZT      ! BUCH     *
      * APA02208!29.01.16! 2.2! STEUERJAHR ERGÄNZT        ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0719                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0719.
          05 PI-DRPA0719-PDPT-KEY.
             10 PI-DRPA0719-PDPT-ID               PIC S9(15)V COMP-3.
      *
       01 PO-DRPA0719.
          05 PO-DRPA0719-RC                       PIC  X(003).
             88 PO-DRPA0719-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0719-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0719-PDPT-DATEN.
             10 PO-DRPA0719-PMPT-ID               PIC S9(15)V COMP-3.
             10 PO-DRPA0719-PMPT-VERSION          PIC S9(4) COMP.
             10 PO-DRPA0719-PEST-ID               PIC S9(15)V COMP-3.
             10 PO-DRPA0719-PDPT-BUCH-RFOLGE      PIC S9(4) COMP.
             10 PO-DRPA0719-PDPT-MA-ABR-DAT       PIC X(10).
             10 PO-DRPA0719-KNR-BUCHUNGS-STAND    PIC S9(4) COMP.
             10 PO-DRPA0719-KNR-BUCHUNGSART       PIC S9(4) COMP.
150898       10 PO-DRPA0719-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.
150898       10 PO-DRPA0719-KNR-KZ-EH-MAX         PIC S9(004) BINARY.
             10 PO-DRPA0719-KNR-BERCH-WAEHRUNG    PIC S9(4) COMP.
             10 PO-DRPA0719-PDPT-DGV              PIC S9(4) COMP.
             10 PO-DRPA0719-PDPT-PRODART          PIC S9(4) COMP.
             10 PO-DRPA0719-PDPT-FIRST-INSERT     PIC X(26).
             10 PO-DRPA0719-PDPT-FIRST-KENNUNG    PIC X(8).
             10 PO-DRPA0719-PDPT-LAST-UPDATE      PIC X(26).
             10 PO-DRPA0719-PDPT-KENNUNG          PIC X(8).
240898       10 PO-DRPA0719-PDPT-PDPE-DATEN.
240898          15 PO-DRPA0719-PDPT-PDPE-DAT-LEN  PIC S9(4) USAGE COMP.
200415*         15 PO-DRPA0719-PDPT-PDPE-DAT-TEXT PIC X(720).
200415          15 PO-DRPA0719-PDPT-PDPE-DAT-TEXT PIC X(1220).
290116       10 PO-DRPA0719-PDPT-UST-JAHR         PIC S9(4) BINARY.
 
 

