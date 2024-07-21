      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0904.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0904.CPv  $
      *
      *   Rev 2.3   Nov 27 2018 07:21:52   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.2   Jan 13 2014 09:48:16   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   Dec 10 2007 11:16:16   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 15:29:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   10 Mar 1998 15:35:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER DRPA0904 :                                     *
      *   FUNKTION DRPA0904 - LIST PM-BESTAND - PO21                 *
      *     DES DIALOGMODULS UGPA0002 (PROVISIONSDATEN)              *
      *   -------------------------------------------------------    *
      *   - BENUTZT BISHER ALS SCHNITTSTELLE ZWISCHEN DEM BUFFER-OZP *
      *     ZYPA0016 ( DIALOG PO21 - PROVISIONSBESTAND - AUSKUNFT    *
      *     PROVISIONSMELDUNGEN ) UND UGPA0002                       *
      *   -------------------------------------------------------    *
      *   ERSTELLT: KUEKENHOEHNER                                    *
      *   DATUM:    22.06.1998                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.06.98! 1.1!NEU                        ! KUEKEN   *
      * APA01526!09.12.13! 2.1!ERWEITERUNG PI-BEREICH     ! STORCH   *
      * APA02721!20.11.18! 2.2!ERWEITERUNG PI-BEREICH     ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0904                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0898 )                *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0904.
          05 PI-DRPA0904-UFKT                    PIC  9(02).
          05 PI-DRPA0904-PRINTE                  PIC S9(015) COMP-3.

          05 PI-DRPA0904-AUSWAHL-KRITERIEN.
             10 PI-DRPA0904-PGEB-GES             PIC 9(002).
             10 PI-DRPA0904-VNR                  PIC X(015).
091213       10 PI-DRPA0904-UNTERNR              PIC S9(004) BINARY.
091213       10 PI-DRPA0904-S-UNTERNR            PIC X(001).
091213       10 PI-DRPA0904-SCHICHTNR            PIC S9(004) BINARY.
091213       10 PI-DRPA0904-S-SCHICHTNR          PIC X(001).
201118       10 PI-DRPA0904-GEL-KENNUNG          PIC X(006).
201118       10 PI-DRPA0904-S-GEL-KENNUNG        PIC X(001).

       01 PO-DRPA0904.
          05  PO-DRPA0904-RC                     PIC  X(003).
              88 PO-DRPA0904-RC-OK                        VALUE 'OK '.
              88 PO-DRPA0904-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA0904-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA0904-ERGEBNISZEILE.
             COPY DRPA0975.

