      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0974.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0974.CPv  $
      *
      *   Rev 2.2   Nov 27 2018 07:21:26   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.1   Jan 13 2014 09:46:52   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.0   May 26 1999 16:37:56   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0974                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0974 DES BUFFER-OZP'S  *
      *    ZYPA0016                                                  *
      *    ( ERGEBNISMENGE X BEREITSTELLEN ,                         *
      *      BISHER BENUTZT VON PGPA0023 IM DIALOG PO21              *
      *      PROVISIONSBESTAND - AUSKUNFT PROVISIONSMELDUNGEN )      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: KUEKENHOEHNER                                   *
      *    DATUM:    20.06.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.06.98!1.1 ! NEU                       ! KUEKEN   *
      * APA02721!20.11.18!2.1 ! ERWEITERUNG PI-BEREICH    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0974
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0973 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0974 DES BUFFER-OZP ZYPA0016 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO21, AUSKUNFT PROVISIONSMELDUNGEN
      *
      ****************************************************************

       01  PI-DRPA0974.
           05  PI-DRPA0974-UFKT                    PIC  9(002).
           05  PI-DRPA0974-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA0974-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA0974-PRINTE                  PIC S9(015)
                                                   PACKED-DECIMAL.
           05  PI-DRPA0974-AUSWAHL-KRITERIEN.
               10 PI-DRPA0974-PGEB-GES             PIC 9(002).
               10 PI-DRPA0974-VNR                  PIC X(015).
091213         10 PI-DRPA0974-UNTERNR              PIC S9(004) BINARY.
091213         10 PI-DRPA0974-S-UNTERNR            PIC X(001).
091213         10 PI-DRPA0974-SCHICHTNR            PIC S9(004) BINARY.
091213         10 PI-DRPA0974-S-SCHICHTNR          PIC X(001).
201118         10 PI-DRPA0974-GEL-KENNUNG          PIC X(006).
201118         10 PI-DRPA0974-S-GEL-KENNUNG        PIC X(001).

       01  PO-DRPA0974.
           05  PO-DRPA0974-RC                      PIC  X(03).
               88 PO-DRPA0974-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0974-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0974-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0974-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA0974-LOG-SATZ-NR-1           PIC  9(05).
      ***   KOMPRIMIERUNG = 'J', WENN ES SICH UM EINEN HAUPTEINTRAG
      ***   HANDELT, DESSEN UNTEREINTRäGE KOMPRIMIERT SIND.
           05  PO-DRPA0974-KZ-KOMPRIMIERUNG        PIC  X(01).
           05  PO-DRPA0974-S-KZ-HISTORIE-PMPG      PIC  X(01).

           05  PO-DRPA0974-ERGEBNISZEILE.
               COPY DRPA0975.
      *
      ******* ENDE COPY-MEMBER **************************************

