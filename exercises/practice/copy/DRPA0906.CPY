      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0906.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0906.CPv  $
      *
      *   Rev 2.6   Nov 27 2018 06:54:38   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.5   Sep 28 2016 10:43:08   t13ex67
      *APA02280: Dialoganpassungen PO23
      *
      *   Rev 2.4   Mar 10 2014 08:51:28   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.3   Jan 13 2014 09:46:34   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.2   Nov 09 2006 12:04:36   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Nov 03 2006 15:35:20   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:29:16   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0906 FUNKTION LIST PM JE PRODUKTTEIL     *
      *                                   FÜR DIALOG PO23            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.05.98! 01 ! NEU                       !BOHLINGER *
      * APA00452!01.11.06! 2.1! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA01526!29.04.13! 2.3! NEUE AUSWAHLKRITERIEN     ! STORCH   *
      *         !07.03.14! 2.4! FOLGENUMMER DREISTELLIG   ! STORCH   *
      * APA02280!19.07.16! 2.5! VB-ABRECHNUNGSDATUM ALS   ! STORCH   *
      * APA02721!24.09.18! 2.6! WEITERES AUSWAHLKRITERIUM ! STORCH   *
      *         !        !    ! GEL-KENNUNG BEI GES. 21   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0906                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0906.
          05 PI-DRPA0906-UFKT                    PIC  9(02).
          05 PI-DRPA0906-PRINTE                  PIC S9(015)
                                                 PACKED-DECIMAL.
          05 PI-DRPA0906-AUSWAHL-KRITERIEN.
             10 PI-DRPA0906-PGEB-GES             PIC 9(002).
             10 PI-DRPA0906-VNR                  PIC X(015).
             10 PI-DRPA0906-FAELLIG-VON          PIC X(010).
             10 PI-DRPA0906-FAELLIG-BIS          PIC X(010).
             10 PI-DRPA0906-S-FAELLIG            PIC X(001).
070314*      10 PI-DRPA0906-FOLGE-NR             PIC 9(002).
070314       10 PI-DRPA0906-FOLGE-NR             PIC 9(003).
             10 PI-DRPA0906-S-FOLGE-NR           PIC X(001).
             10 PI-DRPA0906-PROD-PROD-SCHL       PIC S9(004) BINARY.
             10 PI-DRPA0906-S-PROD-SCHL          PIC X(001).
290413       10 PI-DRPA0906-UNTERNR              PIC S9(004) BINARY.
290413       10 PI-DRPA0906-S-UNTERNR            PIC X(001).
290413       10 PI-DRPA0906-SCHICHTNR            PIC S9(004) BINARY.
290413       10 PI-DRPA0906-S-SCHICHTNR          PIC X(001).
190716       10 PI-DRPA0906-VB-ABR-DAT-VON       PIC X(010).
190716       10 PI-DRPA0906-VB-ABR-DAT-BIS       PIC X(010).
190716       10 PI-DRPA0906-S-VB-ABR-DAT         PIC X(001).
240918       10 PI-DRPA0906-GEL-KENNUNG          PIC X(006).
240918       10 PI-DRPA0906-S-GEL-KENNUNG        PIC X(001).

      *
       01 PO-DRPA0906.
          05  PO-DRPA0906-RC                     PIC  X(003).
              88 PO-DRPA0906-RC-OK                        VALUE 'OK '.
              88 PO-DRPA0906-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA0906-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA0906-DATEN.
             10 PO-DRPA0906-KZ-WEITERE-PROD      PIC  X(001).

          05 PO-DRPA0906-ERGEBNISZEILE.
             COPY DRPA0902.
      *
      ******** Ende Copy-Member **************************************


