      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0953.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0953.CPv  $
      *
      *   Rev 2.5   Nov 27 2018 06:54:50   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.4   Sep 28 2016 10:43:20   t13ex67
      *APA02280: Dialoganpassungen PO23
      *
      *   Rev 2.3   Mar 10 2014 08:51:36   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.2   Jan 13 2014 09:46:42   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   03 Nov 2006 14:20:44   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:41:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0953                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0953                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    12.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.05.98! 01 ! NEU                       ! BOHLINGER*
      * APA00452!03.11.06! 2.2! ERW. WG. 3-STELL. PRODUKTE! WANG     *
      * APA01526!07.03.14! 2.3! FOLGENUMMER DREISTELLIG   ! STORCH   *
      * APA02280!19.07.16! 2.4! VB-ABRECHNUNGSDATUM ALS   ! STORCH   *
      *         !        !    ! WEITERES AUSWAHLKRITERIUM !          *
      * APA02721!24.09.18! 2.5! WEITERES AUSWAHLKRITERIUM ! STORCH   *
      *         !        !    ! GEL-KENNUNG BEI GES. 21   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0953
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0953 DES BUFFER-OZP ZYPA0012 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO23, AUSKUNFT PROVISIONSBEWEGUNGEN
      *
      ****************************************************************
      *
       01  PI-DRPA0953.
           05  PI-DRPA0953-UFKT                    PIC  9(002).
           05  PI-DRPA0953-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA0953-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA0953-PRINTE                  PIC S9(015)
                                                   PACKED-DECIMAL.
           05  PI-DRPA0953-AUSWAHL-KRITERIEN.
               10 PI-DRPA0953-PGEB-GES             PIC 9(002).
               10 PI-DRPA0953-VNR                  PIC X(015).
               10 PI-DRPA0953-FAELLIG-VON          PIC X(010).
               10 PI-DRPA0953-FAELLIG-BIS          PIC X(010).
               10 PI-DRPA0953-S-FAELLIG            PIC X(001).
070314*        10 PI-DRPA0953-FOLGE-NR             PIC 9(002).
070314         10 PI-DRPA0953-FOLGE-NR             PIC 9(003).
               10 PI-DRPA0953-S-FOLGE-NR           PIC X(001).
               10 PI-DRPA0953-PROD-PROD-SCHL       PIC S9(004) BINARY.
               10 PI-DRPA0953-S-PROD-SCHL          PIC X(001).
290413         10 PI-DRPA0953-UNTERNR              PIC S9(004) BINARY.
290413         10 PI-DRPA0953-S-UNTERNR            PIC X(001).
290413         10 PI-DRPA0953-SCHICHTNR            PIC S9(004) BINARY.
290413         10 PI-DRPA0953-S-SCHICHTNR          PIC X(001).
190716         10 PI-DRPA0953-ABR-DAT-VON          PIC X(010).
190716         10 PI-DRPA0953-ABR-DAT-BIS          PIC X(010).
190716         10 PI-DRPA0953-S-ABR-DAT            PIC X(001).
240918         10 PI-DRPA0953-GEL-KENNUNG          PIC X(006).
240918         10 PI-DRPA0953-S-GEL-KENNUNG        PIC X(001).

      *
       01  PO-DRPA0953.
           05  PO-DRPA0953-RC                      PIC  X(03).
               88 PO-DRPA0953-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0953-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0953-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0953-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA0953-LOG-SATZ-NR-1           PIC  9(05).
      ***   KOMPRIMIERUNG = 'J', WENN ES SICH UM EINEN HAUPTEINTRAG
      ***   HANDELT, DESSEN UNTEREINTRÄGE KOMPRIMIERT SIND.
           05  PO-DRPA0953-KZ-KOMPRIMIERUNG        PIC  X(01).
           05  PO-DRPA0953-KZ-WEITERE-PROD         PIC  X(01).

           05  PO-DRPA0953-ERGEBNISZEILE.
               COPY DRPA0902.
      *
      ******* ENDE COPY-MEMBER **************************************


