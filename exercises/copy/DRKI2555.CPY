      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2555.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2555.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:57:28   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:34:08   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:23:52   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:48   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Jan 05 2012 11:05:34   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2555                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2555                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00866!05.01.12! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2555
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT OHNE KUNDENSUCHE  <<<
      *
      *    -für Vertragsbereinigung eventuelles Einfügen in KUNDE mit
      *     Kundennummer
      *    -Insert VN in VERTRAGROLLEBEZ
      *    -Prüfen und ev. einfügen KUNDENINTVBBEZ
      *    -Aktualisieren der PersonenId in KRANKENPERSEIGEN
      *
      ****************************************************************
      *
       01  PI-DRKI2555.
           05  PI-DRKI2555-KEY.
             10 PI-DRKI2555-GES                  PIC  9(002).
             10 PI-DRKI2555-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2555-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2555-ADRESSEID            PIC  X(013).
             10 PI-DRKI2555-KVBVBPERSONENID      PIC  X(013).
             10 PI-DRKI2555-KNDPERSONENID        PIC  X(013).
      *      wenn SPACES, dann auch Ermittlung ID und Insert in Kunde
      *      !!! ohne Kundensuche !!!

           05  PI-DRKI2555-DATEN.
             10 PI-DRKI2555-ANZNAME              PIC  X(030).
             10 PI-DRKI2555-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2555-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2555-GESCHL               PIC  X(001).
             10 PI-DRKI2555-GEBDAT               PIC  X(010).
             10 PI-DRKI2555-KUNDENNUMMER         PIC S9(009) COMP.
             10 PI-DRKI2555-KUNDENSTATUS         PIC  X(001).
      *         I = Interessent
             10 PI-DRKI2555-MANDANTNR            PIC S9(009) COMP.
             10 PI-DRKI2555-GEANDERTPGM          PIC  X(008).
      *         UPKI0185 oder SPACES

             10 PI-DRKI2555-VRBGUELTIGKEITSDAT   PIC  X(010).
             10 PI-DRKI2555-VRBABLAUFDATUM       PIC  X(010).

      *      nur CKV/KRANKENPERSEIGEN:
             10 PI-DRKI2555-PERSONLFDNR          PIC  9(002).

      *
       01  PO-DRKI2555.
      *    Daten
           05  PO-DRKI2555-PERSONENID            PIC  X(013).

      *    Rollen für KRANKENPERSEIGEN
           05  PO-DRKI2555-ROLLE-KPE             PIC  X(002).
               88 PO-DRKI2555-ROLLE-NP           VALUE 'NP'.
      *           Person ist VN u. VP
               88 PO-DRKI2555-ROLLE-N            VALUE 'N '.
      *           Person ist nur VN
      *
      *    Returncode
           05  PO-DRKI2555-RC                    PIC  X(003).
               88 PO-DRKI2555-RC-OK              VALUE 'OK '.
               88 PO-DRKI2555-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2555-RC-NOK             VALUE 'NOK'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2555-FEHLERSATZ.
               10 PO-DRKI2555-FUNKTION           PIC  X(10).
               10 PO-DRKI2555-TABELLE            PIC  X(20).
               10 PO-DRKI2555-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2555-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2555-KEY.
                  15 PO-DRKI2555-GES             PIC  9(002).
                  15 PO-DRKI2555-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
