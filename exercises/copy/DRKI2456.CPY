      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2456.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2456.CPv  $
      *
      *   Rev 2.7   Nov 28 2018 11:52:02   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.6   Sep 25 2015 12:35:16   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.5   Sep 15 2015 15:07:22   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.4   Sep 20 2012 09:56:56   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:38   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:22:54   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:10:06   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Apr 21 2011 07:55:32   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2456                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2456                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00866!21.04.11! 2.1! Neu                       ! WMI      *
      * -------------------------------------------------------------*
010915* AKI01378!01.09.15! 2.2! Neue Felder:              ! DDY      *
010915*         !        !    ! Neuer VN / VNWECHSEL FLAG !          *
010915*         !        !    ! Interne Vertragsnummer    !          *
      * AKI01639!12.11.18!2.7 ! Vornamen in PO-Bereich    ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2456
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2456.
           05  PI-DRKI2456-KEY.
             10 PI-DRKI2456-GES                  PIC  9(002).
             10 PI-DRKI2456-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2456-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2456-ADRESSEID            PIC  X(013).
             10 PI-DRKI2456-VBPERSID             PIC  X(013).
 
           05  PI-DRKI2456-DATEN.
             10 PI-DRKI2456-ANZNAME              PIC  X(030).
             10 PI-DRKI2456-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2456-GEBGRUENDDAT         PIC  X(010).
             10 PI-DRKI2456-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2456-GESCHL               PIC  X(001).
             10 PI-DRKI2456-PLZ                  PIC  X(015).
             10 PI-DRKI2456-ORT                  PIC  X(040).
             10 PI-DRKI2456-STR                  PIC  X(045).
             10 PI-DRKI2456-AUSL-KZ              PIC  X(003).
             10 PI-DRKI2456-KUNDENNUMMER         PIC S9(009) COMP.
             10 PI-DRKI2456-KUNDENSTATUS         PIC  X(001).
      *         K = Kunde, A = Altkunde
             10 PI-DRKI2456-MANDANTNR            PIC S9(009) COMP.
             10 PI-DRKI2456-VERTRAGSTATUS        PIC S9(004) COMP.
010915       10 PI-DRKI2456-VERINTERNEVNR        PIC S9(9) COMP.
             10 PI-DRKI2456-VERTRAGSBEGINN       PIC  X(010).
             10 PI-DRKI2456-VERTRAGSAENDER       PIC  X(010).
             10 PI-DRKI2456-NEUKUNDEDAT          PIC  X(010).
             10 PI-DRKI2456-GEANDERTPGM          PIC  X(008).
             10 PI-DRKI2456-VERTRAG-NEU-ALT      PIC  X(002).
010915       10 PI-DRKI2456-VN-ART               PIC  X(003).
010915          88 PI-DRKI2456-VN-NEU            VALUE 'NEU'.
010915          88 PI-DRKI2456-VN-WECHSEL        VALUE 'VNW'.
010915          88 PI-DRKI2456-VN-MEHRALS1       VALUE 'VNM'.
 
      *
       01  PO-DRKI2456.
           05  PO-DRKI2456-DATEN.
             10 PO-DRKI2456-KUNDENSTATUS         PIC  X(001).
      *         'K' = Kunde, 'A' = Altkunde, ' ' = Neukunde
             10 PO-DRKI2456-PERSONENID           PIC  X(013).
             10 PO-DRKI2456-KNDSUCHVORNAME       PIC  X(030).
             10 PO-DRKI2456-KNDANZVORNAME        PIC  X(030).
             10 PO-DRKI2456-KND-AUS-ANTRAG       PIC  X(001).

           05  PO-DRKI2456-RC                    PIC  X(003).
               88 PO-DRKI2456-RC-OK              VALUE 'OK '.
               88 PO-DRKI2456-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2456-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2456-RC-TU-NICHTS       VALUE 'NOP'.
 
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2456-FEHLERSATZ.
               10 PO-DRKI2456-FUNKTION           PIC  X(10).
               10 PO-DRKI2456-TABELLE            PIC  X(20).
               10 PO-DRKI2456-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2456-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2456-KEY.
                  15 PO-DRKI2456-GES             PIC  9(002).
                  15 PO-DRKI2456-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************


