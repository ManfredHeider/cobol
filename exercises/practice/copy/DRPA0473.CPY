      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0473.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0473.CPv  $
      *
      *   Rev 2.5   16 May 2007 06:43:30   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.4   26 May 2000 15:17:40   T13EX25
      *APA00090, PINZ-Daten neu
      *
      *   Rev 2.3   Jan 28 2000 11:28:22   T13EX44
      * 
      *
      *   Rev 2.2   Apr 21 1999 17:34:36   T13EX22
      *ERGAENZUNGEN FUER PROTOKOLL
      *
      *   Rev 2.1   Mar 11 1999 15:15:02   T13EX22
      *ANZ-DGV ergänzt
      *
      *   Rev 2.0   Mar 10 1999 12:10:54   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:41:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0473                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PROVISION VERWALTEN>>                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEU                       ! MUND     *
      * APA00066!22.11.99!2.01! PE-AENDERUNGS-KZ ENTFERNT ! SCHEIDEL *
      * APA00090!19.05.00!2.02! PINZ-Daten ergänzt        ! Jäger    *
      * APA00604!12.04.07!2.5 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0473                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0473.
          05 PI-DRPA0473-DATEN.
             10 PI-DRPA0473-KNR-B-MODUS        PIC S9(004) BINARY SYNC.
      *
       01 PO-DRPA0473.
          05 PO-DRPA0473-RC                    PIC   X(003).
             88 PO-DRPA0473-RC-OK              VALUE  'OK '.
             88 PO-DRPA0473-RC-VSTAT-GEFUELLT  VALUE  'VSG'.
          05 PO-DRPA0473-VERARB-KZ.
             10 PO-DRPA0473-KZ-NPM-VERARBEITET PIC   X(001).
120407*      10 PO-DRPA0473-KZ-PEW-VORHANDEN   PIC   X(001).
120407       10 PO-DRPA0473-KZ-ANTRAG-VORH     PIC   X(001).
             10 PO-DRPA0473-KZ-ZUKUENFTIG      PIC   X(001).
             10 PO-DRPA0473-KZ-VERARB-FEHLER   PIC   X(001).
221199*      10 PO-DRPA0473-PE-AENDERUNGS-KZ   PIC   X(001).
          05 PO-DRPA0473-ANZ-PNAP.
             10 PO-DRPA0473-ANZ-PNAP-PR        PIC  S9(004) BINARY.
             10 PO-DRPA0473-ANZ-PNAP-PT        PIC  S9(004) BINARY.
             10 PO-DRPA0473-ANZ-PNAP-PM        PIC  S9(004) BINARY.
190499    05 PO-DRPA0473-ANZ-NEUTRAL.
190499       10 PO-DRPA0473-ANZ-PMPE-NEUTRAL   PIC  S9(004) BINARY.
190499       10 PO-DRPA0473-ANZ-PMPP-NEUTRAL   PIC  S9(004) BINARY.
          05 PO-DRPA0473-ANZ-DGV.
             10 PO-DRPA0473-DGV-NOT-USED       PIC  S9(004) BINARY.
             10 PO-DRPA0473-DGV-USED           PIC  S9(004) BINARY.

          05 PO-DRPA0473-DATEN.
120407* DAISY-PEWA-DATEN
120407*      10 PO-DRPA0473-PEW-ANTR.
120407*         COPY DRPA0434.
120407*      10 PO-DRPA0473-PEW-STRUK.
120407*         COPY DRPA0435.
      * DAISY-VERTRAGSDATEN
             10 PO-DRPA0473-PVOR.
                COPY DRPA0614.
             10 PO-DRPA0473-PVER.
                COPY DRPA0408.
190500       10 PO-DRPA0473-PINZ.
190500          COPY DRPA1385.
             10 PO-DRPA0473-PVPT.
                COPY DRPA0420.
             10 PO-DRPA0473-PEST-PEBD.
                COPY DRPA0587.
      * DAISY-BESTANDSMELDUNGEN
             10 PO-DRPA0473-PMBE.
                COPY DRPA0411.
             10 PO-DRPA0473-PMPR.
                COPY DRPA0432.
             10 PO-DRPA0473-PMPT.
                COPY DRPA0421.

      * DAISY-VERTRAGSDATEN-TABELLE
             10 PO-DRPA0473-PVER-TAB.
                COPY DRPA0653.
      * DAISY-BESTANDSMELDUNGEN-TABELLE
             10 PO-DRPA0473-MELD-TAB.
                COPY DRPA0615.
      * DAISY-PMDS-TABELLE
             10 PO-DRPA0473-PMDS.
                COPY DRPA0639.
      ******* ENDE COPY-MEMBER **************************************
