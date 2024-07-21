      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2452.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2452.CPv  $
      *
      *   Rev 2.7   Nov 28 2018 11:51:50   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.6   Sep 25 2015 12:35:12   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.5   Sep 15 2015 15:07:18   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.4   Sep 20 2012 09:56:32   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:33:12   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:22:14   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:09:30   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Apr 21 2011 07:55:04   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2452                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2452                    *
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
      *    FÜR DIE FUNKTION DRKI2452
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2452.
           05  PI-DRKI2452-KEY.
             10 PI-DRKI2452-GES                  PIC  9(002).
             10 PI-DRKI2452-VERTRAGSID           PIC  X(013).
             10 PI-DRKI2452-VERTRAGSNUMMER       PIC  X(015).
             10 PI-DRKI2452-ADRESSEID            PIC  X(013).
             10 PI-DRKI2452-VBPERSID             PIC  X(013).
 
           05  PI-DRKI2452-DATEN.
             10 PI-DRKI2452-ROLLE-VERTRAG        PIC  X(001).
             10 PI-DRKI2452-ROLLE-TARIF          PIC  X(001).
      *         ROLLE-VERTRAG = 'N' und ROLLE-TARIF = SPACE,
      *           Person ist nur VN --> UPKI0224 (VN-Ins/Del)
      *
      *         ROLLE-VERTRAG = 'N' und ROLLE-TARIF = 'P'
      *           Person ist VN u. VP --> UPKI0224 (VN-Ins/Del).
      *                           und --> UPKI0219 (KRANKENPERSEIGEN)
      *         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P'
      *           Person ist nur VP --> AML/UPKI0218 (LEBENPERSONEIGEN)
      *                        oder --> CKV/UPKI0219 (KRANKENPERSEIGEN)
      *
             10 PI-DRKI2452-ANZNAME              PIC  X(030).
             10 PI-DRKI2452-ANZVORNAME           PIC  X(030).
             10 PI-DRKI2452-ANREDEID             PIC S9(004) COMP.
             10 PI-DRKI2452-GESCHL               PIC  X(001).
             10 PI-DRKI2452-GEBDAT               PIC  X(010).
             10 PI-DRKI2452-PLZ                  PIC  X(015).
             10 PI-DRKI2452-ORT                  PIC  X(040).
             10 PI-DRKI2452-STR                  PIC  X(045).
             10 PI-DRKI2452-AUSL-KZ              PIC  X(003).
             10 PI-DRKI2452-KUNDENNUMMER         PIC S9(009) COMP.
             10 PI-DRKI2452-KUNDENSTATUS         PIC  X(001).
      *         K = Kunde, A = Altkunde
             10 PI-DRKI2452-MANDANTNR            PIC S9(009) COMP.
             10 PI-DRKI2452-VERTRAGZUSTANDID     PIC S9(004) COMP.
010915       10 PI-DRKI2452-VERINTERNEVNR        PIC S9(9) COMP.
             10 PI-DRKI2452-VERTRAGSBEGINN       PIC  X(010).
             10 PI-DRKI2452-VERTRAGSAENDER       PIC  X(010).
             10 PI-DRKI2452-NEUKUNDEDAT          PIC  X(010).
             10 PI-DRKI2452-GEANDERTPGM          PIC  X(008).
             10 PI-DRKI2452-VERTRAG-NEU-ALT      PIC  X(002).
 
      *      nur AML/LEBENPERSONEIGEN:
             10 PI-DRKI2452-VTRBEZ-ARTKZ         PIC  X(001).
      *         ROLLE-VERTRAG = 'Z'
      *           Person ist Beitrags'Z'ahler
      *         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P'
      *           VTRBEZ-ARTKZ = SPACE
      *             'H'auptversichete Person
      *           VTRBEZ-ARTKZ = 'M'
      *             'M'itversicherte Person
      *           VTRBEZ-ARTKZ = '2'
      *             '2'. mitversicherte Person
      *
      *      nur CKV/KRANKENPERSEIGEN:
             10 PI-DRKI2452-PERSONLFDNR          PIC  9(002).
010915       10 PI-DRKI2452-VN-ART               PIC  X(003).
010915          88 PI-DRKI2452-VN-NEU            VALUE 'NEU'.
010915          88 PI-DRKI2452-VN-WECHSEL        VALUE 'VNW'.
010915          88 PI-DRKI2452-VN-MEHRALS1       VALUE 'VNM'.
 
      *
       01  PO-DRKI2452.
      *    Daten
           05  PO-DRKI2452-PERSONENID            PIC  X(013).
           05  PO-DRKI2452-KNDSTATUSID           PIC  X(001).
           05  PO-DRKI2452-KNDSUCHVORNAME        PIC  X(030).
           05  PO-DRKI2452-KNDANZVORNAME         PIC  X(030).
      *        'K' = Kunde, 'A' = Altkunde, ' ' Neukunde
           05  PO-DRKI2452-KND-AUS-ANTRAG        PIC  X(001).
 
      *    Rollen für LEBENPERSONEIGEN
           05  PO-DRKI2452-ROLLE-LPE             PIC  X(001).
               88 PO-DRKI2452-ROLLE-H            VALUE 'H'.
      *           'H'auptversichete Person
               88 PO-DRKI2452-ROLLE-M            VALUE 'M'.
      *           'M'itversicherte Person
               88 PO-DRKI2452-ROLLE-2M           VALUE '2'.
      *           '2'. mitversicherte Person
               88 PO-DRKI2452-ROLLE-BZ           VALUE 'Z'.
      *           Person ist Beitrags'Z'ahler
      *
      *    Rollen für KRANKENPERSEIGEN
           05  PO-DRKI2452-ROLLE-KPE             PIC  X(002).
               88 PO-DRKI2452-ROLLE-NP           VALUE 'NP'.
      *           Person ist VN u. VP
               88 PO-DRKI2452-ROLLE-N            VALUE 'N '.
      *           Person ist nur VN
               88 PO-DRKI2452-ROLLE-P            VALUE ' P'.
      *           Person ist nur VP
      *
      *    Returncode
           05  PO-DRKI2452-RC                    PIC  X(003).
               88 PO-DRKI2452-RC-OK              VALUE 'OK '.
               88 PO-DRKI2452-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2452-RC-NOK             VALUE 'NOK'.
 
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2452-FEHLERSATZ.
               10 PO-DRKI2452-FUNKTION           PIC  X(10).
               10 PO-DRKI2452-TABELLE            PIC  X(20).
               10 PO-DRKI2452-SQL-FEHLER         PIC S9(04).
               10 PO-DRKI2452-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2452-KEY.
                  15 PO-DRKI2452-GES             PIC  9(002).
                  15 PO-DRKI2452-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************


