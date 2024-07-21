      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2554.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2554.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:57:22   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:34:02   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:23:40   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:40   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Jan 05 2012 07:50:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2554                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2554                    *
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
      *    FÜR DIE FUNKTION DRKI2554
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
       01  PI-DRKI2554.
           05  PI-DRKI2554-KEY.
             10 PI-DRKI2554-GES                  PIC  9(002).
             10 PI-DRKI2554-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2554-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2554-ADRESSEID            PIC  X(013).
             10 PI-DRKI2554-KVBVBPERSONENID      PIC  X(013).
             10 PI-DRKI2554-KNDPERSONENID        PIC  X(013).
      *      wenn SPACES, dann auch Ermittlung ID und Insert in Kunde
      *      !!! ohne Kundensuche !!!

           05  PI-DRKI2554-DATEN.
             10 PI-DRKI2554-ROLLE-VERTRAG        PIC  X(001).
      *         ROLLE-VERTRAG = 'N'
      *           Person ist VN --> UPKI0224 (VN-Ins/Del)
      *
             10 PI-DRKI2554-ANZNAME              PIC  X(030).
             10 PI-DRKI2554-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2554-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2554-GESCHL               PIC  X(001).
             10 PI-DRKI2554-GEBDAT               PIC  X(010).
             10 PI-DRKI2554-KUNDENNUMMER         PIC S9(009) COMP.
             10 PI-DRKI2554-KUNDENSTATUS         PIC  X(001).
      *         I = Interessent
             10 PI-DRKI2554-MANDANTNR            PIC S9(009) COMP.
             10 PI-DRKI2554-GEANDERTPGM          PIC  X(008).
      *         UPKI0185 oder SPACES

             10 PI-DRKI2554-VRBGUELTIGKEITSDAT   PIC  X(010).
             10 PI-DRKI2554-VRBABLAUFDATUM       PIC  X(010).

      *
       01  PO-DRKI2554.
      *    Daten
           05  PO-DRKI2554-PERSONENID            PIC  X(013).

      *    Rollen für KRANKENPERSEIGEN
           05  PO-DRKI2554-ROLLE-KPE             PIC  X(002).
               88 PO-DRKI2554-ROLLE-NP           VALUE 'NP'.
      *           Person ist VN u. VP
               88 PO-DRKI2554-ROLLE-N            VALUE 'N '.
      *           Person ist nur VN
      *
      *    Returncode
           05  PO-DRKI2554-RC                    PIC  X(003).
               88 PO-DRKI2554-RC-OK              VALUE 'OK '.
               88 PO-DRKI2554-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2554-RC-NOK             VALUE 'NOK'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2554-FEHLERSATZ.
               10 PO-DRKI2554-FUNKTION           PIC  X(10).
               10 PO-DRKI2554-TABELLE            PIC  X(20).
               10 PO-DRKI2554-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2554-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2554-KEY.
                  15 PO-DRKI2554-GES             PIC  9(002).
                  15 PO-DRKI2554-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
