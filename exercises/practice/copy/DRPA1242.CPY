      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1242.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1242.CPv  $
      *
      *   Rev 2.4   16 May 2007 06:45:00   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.3   25 May 2000 11:09:44   T13EX21
      *APA00090 - Erweiterung um PINZ
      *
      *   Rev 2.2   Jan 19 2000 08:48:56   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.1   Jun 25 1999 11:20:30   T13EX22
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:12   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1242                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DAISY-DATENBANK AKTUALISIEREN>>          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    20.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.05.99! 01 ! NEU                       ! MUND     *
      * APA00063!29.10.99! 02 ! UFKT UND S-VERARBEITUNG   ! STORCH   *
      *         !        !    ! AUS PI-BEREICH ENTFERNT   !          *
      * APA00090!24.05.00! 03 ! ERWEITERUNG UM PINZ       ! STORCH   *
      * APA00604!12.04.07!2.4 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1242                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1242.
          05 PI-DRPA1242-STEUER-DATEN.
291099*      10 PI-DRPA1242-UFKT                      PIC 9(002).

             10 PI-DRPA1242-S-MODUS                   PIC 9.
                88 PI-DRPA1242-MODUS-LANG             VALUE 1.
                88 PI-DRPA1242-MODUS-KORREKTUR        VALUE 3.

             10 PI-DRPA1242-S-VERARBEITET             PIC X.
                88 PI-DRPA1242-NPM-VERARB             VALUE 'J'.
                88 PI-DRPA1242-NPM-NOT-VERARB         VALUE 'N'.
                88 PI-DRPA1242-NPM-NEUTRAL-VERARB     VALUE 'V'.
120407*
120407*      10 PI-DRPA1242-S-PEW-DA                  PIC X.
120407*         88 PI-DRPA1242-PEW-FOUND              VALUE 'J'.
120407*         88 PI-DRPA1242-PEW-NOT-FOUND          VALUE 'N'.
      *
120407       10 PI-DRPA1242-S-ANTRAG                  PIC X.
120407          88 PI-DRPA1242-ANTRAG-FOUND           VALUE 'J'.
120407          88 PI-DRPA1242-ANTRAG-NOT-FOUND       VALUE 'N'.

             10 PI-DRPA1242-S-ZUKUENFTIG              PIC X.
                88 PI-DRPA1242-ZUKUENFTIG             VALUE 'J'.
                88 PI-DRPA1242-NICHT-ZUKUENFTIG       VALUE 'N'.

             10 PI-DRPA1242-S-V-FEHLER                PIC X.
                88 PI-DRPA1242-VERARB-OK              VALUE 'N'.
                88 PI-DRPA1242-VERARB-FEHLER          VALUE 'J'.

291099*      10 PI-DRPA1242-S-VERARBEITUNG            PIC 9.
291099*         88 PI-DRPA1242-ABBRECHEN              VALUE 0.
291099*         88 PI-DRPA1242-NORMAL                 VALUE 1.
291099*         88 PI-DRPA1242-FOLGESATZ              VALUE 2.

          05 PI-DRPA1242-DATEN.
120407* DAISY-ANTRAGS-DATEN
120407*      10 PI-DRPA1242-PEW-ANTRAG.
120407*         COPY DRPA0434.
120407*      10 PI-DRPA1242-PEW-STRUKTUR.
120407*         COPY DRPA0435.
      * DAISY-NPM-DATEN
             10 PI-DRPA1242-PMPE.
                COPY DRPA0289.
             10 PI-DRPA1242-PMPP.
                COPY DRPA0290.
      * DAISY-VERTRAGSDATEN
             10 PI-DRPA1242-PVOR.
                COPY DRPA0614.
             10 PI-DRPA1242-PVER.
                COPY DRPA0408.
240500       10 PI-DRPA1242-PINZ.
240500          COPY DRPA1385.
             10 PI-DRPA1242-PVPT.
                COPY DRPA0420.
             10 PI-DRPA1242-PEST-PEBD.
                COPY DRPA0587.
      * DAISY-BESTANDSMELDUNGEN
             10 PI-DRPA1242-PMBE.
                COPY DRPA0411.
             10 PI-DRPA1242-PMPR.
                COPY DRPA0432.
             10 PI-DRPA1242-PMPT.
                COPY DRPA0421.

      * DAISY-TABELLE MIT VORVERTRÄGEN AUS GEVO-EVS
             10 PI-DRPA1242-PVER-TAB.
                COPY DRPA0653.
      * DAISY-TABELLE MIT RÜCKBELASTUNGSMELDUNGEN AUS GEVO-EVS
             10 PI-DRPA1242-MELD-TAB.
                COPY DRPA0615.
      * DAISY-PMDS-TABELLE
             10 PI-DRPA1242-PMDS.
                COPY DRPA0639.
      *
       01 PO-DRPA1242.
          05 PO-DRPA1242-RC                    PIC   X(003).
             88 PO-DRPA1242-RC-OK              VALUE  'OK '.
      *   ZÄHLER
          05 PO-DRPA1242-ZAEHLER.
             10  PO-DRPA1242-PVER-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PVPT-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PMBE-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PMPR-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PMPT-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PVOR-INSERT   PIC S9(009)  BINARY.
240500       10  PO-DRPA1242-PINZ-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PDPT-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PEST-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PAEV-INSERT   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PVPT-UPDATE   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PMDS-INSERT   PIC S9(009)  BINARY.
120407*      10  PO-DRPA1242-PEWA-UPDATE   PIC S9(009)  BINARY.
             10  PO-DRPA1242-PVER-UPDATE   PIC S9(009)  BINARY.
             10  PO-DRPA1242-ARBEITSSAETZE PIC S9(009)  BINARY.
             10  PO-DRPA1242-PMPE-SAVED    PIC S9(009)  BINARY.
      ********** ENDE COPY-MEMBER **************************************
