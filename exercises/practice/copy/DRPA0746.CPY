      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0746.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0746.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:58:00   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Jul 16 2015 15:08:06   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   Nov 05 1999 12:01:22   T13EX21
      *
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0746  FUNKTION INSERT 2 STANDARD-OZP     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    05.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00107!05.11.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA01932!20.04.15! 2.1! UMSATZSTEUER EINGEFÜHRT   ! BUCH     *
      * APA02208!29.01.16! 2.2! STEUERJAHR ERGÄNZT        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0746                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT 2 <<<       TABELLE PDPT TBPA0122             *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0746.
          05 PI-DRPA0746-PROGRAMM                 PIC  X(008).
          05 PI-DRPA0746-UFKT                     PIC  9(002).
          05 PI-DRPA0746-PDPT-DATEN.
             10 PI-DRPA0746-PDPT-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0746-PDPT-KENNUNG          PIC X(8).
             10 PI-DRPA0746-PMPT-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0746-PMPT-VERSION          PIC S9(4) COMP.
             10 PI-DRPA0746-PEST-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0746-PDPT-BUCH-RFOLGE      PIC S9(4) COMP.
             10 PI-DRPA0746-PDPT-MA-ABR-DAT       PIC X(10).
             10 PI-DRPA0746-KNR-BUCHUNGS-STAND    PIC S9(4) COMP.
             10 PI-DRPA0746-KNR-BUCHUNGSART       PIC S9(4) COMP.
             10 PI-DRPA0746-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.
             10 PI-DRPA0746-KNR-KZ-EH-MAX         PIC S9(004) BINARY.
             10 PI-DRPA0746-KNR-BERCH-WAEHRUNG    PIC S9(4) COMP.
             10 PI-DRPA0746-PDPT-DGV              PIC S9(4) COMP.
             10 PI-DRPA0746-PDPT-PRODART          PIC S9(4) COMP.
             10 PI-DRPA0746-PDPT-FIRST-INSERT     PIC X(26).
             10 PI-DRPA0746-PDPT-FIRST-KENNUNG    PIC X(8).
             10 PI-DRPA0746-PDPT-PDPE-DATEN.
                15 PI-DRPA0746-PDPT-PDPE-DAT-LEN  PIC S9(4) USAGE COMP.
200415*         15 PI-DRPA0746-PDPT-PDPE-DAT-TEXT PIC X(720).
200415          15 PI-DRPA0746-PDPT-PDPE-DAT-TEXT PIC X(1220).
290116       10 PI-DRPA0746-PDPT-UST-JAHR         PIC S9(4) BINARY.
      *
       01 PO-DRPA0746.
          05 PO-DRPA0746-RC                       PIC  X(003).
             88 PO-DRPA0746-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0746-RC-DUPLICATE-VALUE            VALUE 'DVA'.
 
 

