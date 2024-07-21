      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2347.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2347.CPv  $
      *
      *   Rev 2.5   Mar 27 2017 09:02:22   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.4   Jul 02 2012 10:36:02   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.3   Jun 29 2011 07:55:30   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.2   Oct 27 2009 12:21:20   t13ex50
      *
      *   Rev 2.1   Aug 20 2009 15:24:38   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 07 2009 14:06:22   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 3 4 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER GES. DeuBa,       *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: ALLE EIN- UND AUSZAHLUNGEN                   *
      *                                                                *
      *    SATZLÄNGE   350                                             *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM    !      ÄNDERUNG                           !NAME        *
      *AUFTRAG  !                                         !            *
      *---------+--------+--------------------------------+------------*
      * AKI00844!03.08.09!2.00! Neuanlage                 !SCHLEICHER  *
      *---------+--------+----+---------------------------+------------*
100612* AKI01149!11.06.12!5.0 !PIN verarbeiten            !WHO        *
      *---------+--------+----+---------------------------+------------*
      * FKI01544!22.03.17!    ! III entfernt              !HHU         *
      *         !        !    !                           !            *
      ******************************************************************
       01  DRKI2347.
           05  GZ-GES                               PIC  9(002).
           05  GZ-VNR                               PIC  9(015).
 
           05  GZ-ZAHLUNGS-KZ                       PIC  X(001).
      *        'E' = Einzahlung, 'A' = Auszahlung
 
           05  GZ-DEPOT-UNR                         PIC  9(002).
      *        Depotunternr. wird Teil des Keys in Guthabenprodukt
 
           05  GZ-ZAHLUNG-LFDNR                     PIC  9(004).
      *        LFDNR von DIT und Sparplannr. von DB
 
           05  GZ-PRODUKT-ID                        PIC  9(003).
      *        PRODUKT-ID
 
           05  GZ-ISIN                              PIC  X(012).
      *        Internationale WKN
 
           05  GZ-WP-KENN-NR                        PIC  X(006).
      *        WERTPAPIERKENNNUMMER
 
           05  GZ-BETRAG                            PIC  9(007)V99.
      *        Sparleistung oder Auszahlung
           05  GZ-DYNAM                             PIC  9(002)V99.
 
           05  GZ-ZAHLWEISE                         PIC S9(002).
 
           05  GZ-TAG                               PIC  9(002).
      *        Tag der Ein- oder Auszahlung
 
           05  GZ-BUCHSCHL                          PIC S9(002).
 
           05  GZ-DAT-EROEFFNUNG                    PIC  X(010).
      *        Datum Sparplan-/Auszahlplaneröffnung
 
           05  GZ-DAT-SCHLIESSUNG                   PIC  X(010).
      *        Datum Sparplan-/ Auszahlplanschliessung
 
           05  GZ-KONTOINHABER                      PIC  X(027).
      *        Kontoinhaber von DB
 
           05  GZ-BANKNAME                          PIC  X(020).
      *        Bankverbindung von DB, Namen des Instituts
 
           05  GZ-BLZ                               PIC  9(011).
      *        Bankverbindung von DB, BLZ des Instituts
 
           05  GZ-KONTONR                           PIC  9(018).
      *        Bankverbindung von DB, Kontonr.
 
           05  GZ-AKTUAL-KZ                         PIC  X(001).
      *        I = INSERT
      *        D = DELETE
      *        U = UPDATE
 
           05  GZ-ENDE-RATENAUSSETZUNG              PIC  X(010).
      *            Endedatum der Ratenaussetzung
           05  GZ-DAT-VERTR-AUFLOESUNG              PIC  X(010).
      *               Datum Vertragsauflösung  bzw. vorzeitige
      *               Vertragskündigung
      *
           05  GZ-VERTRAGSTYP                       PIC  X(002).
 
           05  GZ-MAHNSTATUS                        PIC  X(002).
      *
           05  GZ-DAUERZULAGE-KZ                    PIC  X(001).
      *
250511     05  GZ-KZ-UNM-ZULAGEBER                PIC X(001).
      *              KZ UNMITTELBARE ZULAGENBERECHTIGUNG
 
           05   GZ-KZ-UEBWEG-KAPUEB                PIC X(001).
      *              KZ ÜBERTRAGUNGSWEG KAPITALÜBERTRAGUNG
 
           05   GZ-KZ-UEBART                       PIC X(001).
      *              KZ ÜBERTRAGUNGSART
 
           05   GZ-DAT-KAPITAL-UEBERTRAG           PIC X(010).
      *              DATUM KAPITAL ÜBERTRAG
 
           05   GZ-DAT-GELD-BEWEG                  PIC X(010).
      *              DATUM GELD BEWEGUNG
 
           05   GZ-ANZAHL-KINDZULAGE               PIC 9(002).
      *               ANZAHL KINDERZULAGEN IM ANTRAG
 
           05  GZ-GARANTIESUMME                   PIC 9(15)V99.
      *               GARANTIESUMME
 
           05  GZ-DAT-BEGINN-ABLSTAB              PIC X(010).
      *              DATUM BEGINN ABLAUFSTABILISIERUNG
 
           05  GZ-DAT-BEGINN-HOECHSICH            PIC X(010).
      *              DATUM BEGINN HOECHSTSTANDSICHERUNG
 
           05  GZ-DAT-LETZTHOECHST                PIC X(010).
      *              DATUM LETZTE HOECHSTSTANDANPASSUNG
 
           05  GZ-BETRHOECHST                     PIC 9(15)V99.
      *              HÖCHSTSTAND
 
           05  GZ-DAT-KUENDIGUNG                  PIC X(010).
      *              DATUM KUENDIGUNGSTERMIN
100612     05  GZ-PIN                             PIC 9(009).
100612*    05  FILLER                             PIC X(065).
100612     05  FILLER                             PIC X(056).
 

