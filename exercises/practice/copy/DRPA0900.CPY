      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0900.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0900.CPv  $
      *
      *   Rev 2.4   Nov 27 2018 06:54:24   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.3   Mar 10 2014 08:51:10   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.2   Jan 13 2014 09:46:10   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   03 Nov 2006 14:20:56   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:29:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0900 FUNKTION GET PROVISIONSDETAIL       *
      *                                                              *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    09.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!09.04.98! 01 !NEU                        !BOHLINGER *
      * APA00452!03.11.06! 2.1!ERW. WG. 3-STELL. PRODUKTE !WANG      *
      * APA01526!10.05.13! 2.2! UNTERNR. UND SCHICHTNR.   !STORCH    *
      *         !07.03.14! 2.3! FOLGENUMMER DREISTELLIG   !STORCH    *
      * APA02721!24.09.18! 2.4! NEU: GEL-KENNUNG          !STORCH    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0900                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0900.
             10 PI-DRPA0900-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0900-AUSWAHL-KRITERIEN.
             10 PI-DRPA0900-PGEB-GES           PIC 9(002).
             10 PI-DRPA0900-VNR                PIC X(015).
             10 PI-DRPA0900-FAELLIG-VON        PIC X(010).
             10 PI-DRPA0900-FAELLIG-BIS        PIC X(010).
             10 PI-DRPA0900-S-FAELLIG          PIC X(001).
070314*      10 PI-DRPA0900-FOLGE-NR           PIC 9(002).
070314       10 PI-DRPA0900-FOLGE-NR           PIC 9(003).
             10 PI-DRPA0900-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA0900-ORDN-NR            PIC 9(002).
             10 PI-DRPA0900-HIST-NR            PIC S9(004) BINARY.
             10 PI-DRPA0900-S-HIST-NR          PIC  X(001).
100513       10 PI-DRPA0900-UNTER-NR           PIC  X(003).
100513       10 PI-DRPA0900-S-UNTER-NR         PIC  X(001).
100513       10 PI-DRPA0900-SCHICHT-NR         PIC  X(003).
100513       10 PI-DRPA0900-S-SCHICHT-NR       PIC  X(001).
240918       10 PI-DRPA0900-GEL-KENNUNG        PIC  X(006).
240918       10 PI-DRPA0900-S-GEL-KENNUNG      PIC  X(001).

      *
       01 PO-DRPA0900.
          05 PO-DRPA0900-RC                    PIC  X(003).
             88 PO-DRPA0900-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0900-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA0900-MELDUNG.
             10 PO-DRPA0900-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0900-MELDUNG-NR         PIC S9(004) BINARY.

      *** MAPSTEUERUNG DER DATENFELDER
          05 PO-DRPA0900-MAP-STEUERUNG.
             COPY DRPA0930.

          05 PO-DRPA0900-DATEN.
             COPY DRPA0901.
      *
      ******** Ende Copy-Member **************************************

