      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0166.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0166.CPv  $
      *
      *   Rev 2.7   Feb 17 2011 09:39:14   t13ae34
      *AAN00984: Neuer Tarif 1RG - renditeoptimiert + -EU Rel. 5.28
      *
      *   Rev 2.6   Aug 15 2007 10:33:52   t13ae34
      *FAN00768: FR11611: AML Eigenvertragskenneichen TDS-Sätze
      *
      *   Rev 2.5   Apr 10 2007 22:13:28   t13ae34
      *FAN00706: FR8004, FR8031, Kennz.BP in TDS, Umnummerierte Ant
      *räge
      *
      *   Rev 2.4   11 Dec 2006 15:01:00   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   04 Dec 2006 13:18:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   08 Nov 2006 14:20:04   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   07 Nov 2006 16:43:28   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER  WORKFLOW ANTRAGS-TDS
      *    --------------------------------------
      *
      *    LAENGE = 682 BYTES
      *
      *    BEMERKUNG:
      *    IST ZU VERWENDEN MIT DRAN0163, WELCHES DIE STUFENNUMMER
      *    01 BEINHALTET.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!07.11.06!2.1  ! NEU                      ! HEIDER   *
      * AAN00655!11.11.06!2.2  ! UNTERTEILUNG             ! HEIDER   *
      * AAN00655!10.04.07!2.5  ! KENNZEICHEN BERATERPROT. ! HEIDER   *
      * FAN00768!15.08.07!2.6  ! EIGENVERTRAGSKENNEICHEN  ! HEIDER   *
      * AAN00984!11.02.11!2.7  ! AML: EU-TARIFE           ! HEIDER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
           05  WF-ANTRAG-TDS-SATZ.

           07  WF-ANTRAG-TDS-HEADER.
      *----AN-TDS-SPEZIFISCHER TEIL (22 BYTE)
      *
             10  TDS-MODUS                PIC  X(002).
             10  TDS-DATUM                PIC  9(007)    COMP-3.
             10  TDS-UHR                  PIC  9(009)    COMP-3.
             10  TDS-TERMINAL-ID          PIC  X(004).
             10  TDS-BEARBEITER-ID        PIC  X(007).
      *
      *----AB HIER ABBILD DES ANTRAGSSATZES (660 BYTE)
      *
           07  WF-ANTRAG-SATZ.

             10  TDS-SATZART              PIC  9(002).
             10  TDS-KEY.
               20  TDS-AVERM              PIC  9(007)    COMP-3.
               20  TDS-PGEB-GES           PIC  9(002).
               20  TDS-NR                 PIC  9(015)    COMP-3.
      *
             10  TDS-SATZ.
               20  TDS-AVERM-PZ           PIC  X(001).
               20  TDS-PIN-PZ             PIC  X(002).
               20  TDS-MC-KUNDE-NAME      PIC  X(008).
               20  TDS-VM                 PIC  9(007)    COMP-3.
               20  TDS-VM-PZ              PIC  X(001).
      **
               20  TDS-PROD-PROD-SCHL.

                   25  TDS-PROD-PROD-SCHL-1   PIC  9(003)    COMP-3.
      *
      * PROD-2     BEI PGEB-GES= 20 (AML) IMMER LEER
                   25  TDS-PROD-PROD-SCHL-2   PIC  9(003)    COMP-3.
      *
      * PROD-3     BEI PGEB-GES= 20 (AML) -> PS 36+37 AUS ELEKTR.ANT
                   25  TDS-PROD-PROD-SCHL-3   PIC  9(003)    COMP-3.
      *
      * PROD-4     BEI PGEB-GES= 20 (AML) -> PS 39 AUS ELEKTR.ANTRAG
                   25  TDS-PROD-PROD-SCHL-4   PIC  9(003)    COMP-3.
      *
      * PROD-5     BEI PGEB-GES= 20 (AML) -> PS 38 AUS ELEKTR.ANTRAG
                   25  TDS-PROD-PROD-SCHL-5   PIC  9(003)    COMP-3.
      *
      * PROD-6     BEI PGEB-GES= 20 (AML) -> PS 17 AUS ELEKTR.ANTRAG
                   25  TDS-PROD-PROD-SCHL-6   PIC  9(003)    COMP-3.
      *
      * PROD-7     BEI PGEB-GES= 20 (AML) -> PS 17,18,97,98
                   25  TDS-PROD-PROD-SCHL-7   PIC  9(003)    COMP-3.
      *
      * PROD-8     BEI PGEB-GES= 20 (AML) -> PS 121,122 EU-ZUSATZ
                   25  TDS-PROD-PROD-SCHL-8   PIC  9(003)    COMP-3.
      *
                   25  TDS-PROD-PROD-SCHL-9   PIC  9(003)    COMP-3.
      *
               20  TDS-PROD-PROD-SCHL-TABELLE REDEFINES
                   TDS-PROD-PROD-SCHL.

                   25 TDS-PROD-PROD-SCHL-TAB  PIC 9(003) COMP-3
                                              OCCURS 9.

      **
               20  TDS-TARIF                  PIC  X(010).
      *
      *            BEI AGES=38 -> ZAHLWEISE JE PRODUKT
      *            BEI AGES=39 -> ZAHLWEISE JE PRODUKT
      *            BEI AGES=40 -> ZAHLWEISE JE PRODUKT
      *            BEI AGES=48 -> ZAHLWEISE JE PRODUKT
               20  TDS-ZAHLWEISE-TABELLE  REDEFINES TDS-TARIF.
                   25 TDS-ZAHLWEISE-TAB   PIC  9(1)      OCCURS 9.
                   25 FILLER              PIC  X(1).
      *
               20  TDS-ZAHLWEISE-TABELLE-X REDEFINES TDS-TARIF.
                   25 TDS-ZAHLWEISE-TAB-X PIC  X(1)     OCCURS 9.
                   25 FILLER              PIC  X(1).
      *
      *            WENN PGEB-GES=20 -> KENNZ. DIREKTVERS. J/N
      *            EX-FELD ANREDE (ABANREDE)
               20  TDS-DIREKTVERS           PIC  X(001).
      *
               20  TDS-KUNDE-VNAME          PIC  X(026).
      *
               20  TDS-KUNDE-NAME           PIC  X(026).

      *            EX-FELD-STRASSE (ABSTR)
               20  TDS-EX-STRASSE-FELD      PIC  X(026).
      **
               20  FILLER                 REDEFINES
                   TDS-EX-STRASSE-FELD.
      *            BEI PGEB-GES=10 (AMV) -> EFFEKTIVER JAHRESBEITRAG
      *            BEI PGEB-GES=20 (AML) -> BEITRÄGE ZUSATZVERSICHER
      *            LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
      *            (PRODUKTE 1-6)
      *            ------------------------------------------------
      *            BEI PGEB-GES=20 (AML) -> PENKA: ENDE HV-DAUER
                   25  TDS-BEITRAG-EFF-1    PIC S9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> PENKA: ENDE BZ-DAUER
                   25  TDS-BEITRAG-EFF-2    PIC S9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> PS 36+37 AUS ELEKTR.ANT
                   25  TDS-BEITRAG-EFF-3    PIC S9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> PS 39 AUS ELEKTR.ANTRAG
                   25  TDS-BEITRAG-EFF-4    PIC S9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> PS 18 AUS ELEKTR.ANTRAG
                   25  TDS-BEITRAG-EFF-5    PIC S9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> PS 17 AUS ELEKTR.ANTRAG
                   25  TDS-BEITRAG-EFF-6    PIC S9(007)    COMP-3.
      *
                   25  FILLER               PIC  X(002).
      *
               20  TDS-BEITRAG-EFF-TABELLE REDEFINES
                   TDS-EX-STRASSE-FELD.

                   25 TDS-BEITRAG-EFF-TAB   PIC S9(007) COMP-3
                                            OCCURS 6.
      *
                   25  FILLER               PIC  X(002).
      **
      *
               20  TDS-AUSL                 PIC  X(003).
      *
               20  TDS-PLZ                  PIC  X(009).
      *
               20  TDS-ORT                  PIC  X(021).
      *
               20  TDS-EALTER               PIC  9(002).
      *
               20  TDS-DAUER                PIC  9(002).
      *
               20  TDS-VERS-BEGINN          PIC  9(007)    COMP-3.
      *
               20  TDS-VERS-ENDE            PIC  9(007)    COMP-3.
      *
      *        ZAHLWEISE AUßER LAUZEITABHÄNGIGE PRODUKTE
               20  TDS-ZAHLWEISE            PIC  9(001).
      *
               20  TDS-BUZPROZ              PIC  9(002)    COMP-3.
      **
               20  TDS-EINH-AVERM.
      *
                   25  TDS-EINH-AVERM-1     PIC S9(006)V9  COMP-3.
      *
      * EINH-2 BEI GES.   20 AML IMMER LEER
                   25  TDS-EINH-AVERM-2     PIC S9(006)V9  COMP-3.
      *
      * EINH-3 BEI PGEB-GES= 20 (AML) -> PS 36+37 AUS ELEKTR.ANTRAG
                   25  TDS-EINH-AVERM-3     PIC S9(006)V9  COMP-3.
      *
      * EINH-4 BEI PGEB-GES= 20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
                   25  TDS-EINH-AVERM-4     PIC S9(006)V9  COMP-3.
      *
      * EINH-5 BEI PGEB-GES= 20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
                   25  TDS-EINH-AVERM-5     PIC S9(006)V9  COMP-3.
      *
      * EINH-6 BEI PGEB-GES= 20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
                   25  TDS-EINH-AVERM-6     PIC S9(006)V9  COMP-3.
      *
                   25  TDS-EINH-AVERM-7     PIC S9(006)V9  COMP-3.
      *
                   25  TDS-EINH-AVERM-8     PIC S9(006)V9  COMP-3.
      *
                   25  TDS-EINH-AVERM-9     PIC S9(006)V9  COMP-3.
      *
               20  TDS-EINH-AVERM-TABELLE REDEFINES
                   TDS-EINH-AVERM.
      *
                   25 TDS-EINH-AVERM-TAB PIC S9(006)V9
                                         COMP-3 OCCURS 9.
      **
      **
               20  TDS-SUMME.
      *
                   25  TDS-SUMME-1        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-2        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-3        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-4        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-5        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-6        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-7        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-8        PIC S9(007)    COMP-3.
      *
                   25  TDS-SUMME-9        PIC S9(007)    COMP-3.
      *
               20  TDS-TDS-SUMME-TABELLE REDEFINES
                   TDS-SUMME.
      *
                   25 TDS-SUMME-TAB       PIC S9(007)
                                            COMP-3 OCCURS 9.
      **
               20  TDS-BEITRAG              PIC S9(007)V99 COMP-3.
      *
               20  TDS-APEINH               PIC S9(006)V9  COMP-3.
      *
               20  TDS-APSUMME              PIC S9(007)    COMP-3.
      *
               20  TDS-APBEITRAG            PIC S9(005)V99 COMP-3.
      *
      *            BEARBEITUNGSGEBÜR GES. 85 (BAWAG) IN PROZENT
               20  TDS-ANBEARBEITGEB-BAWAG  REDEFINES TDS-APBEITRAG
                                            PIC S9(5)V99   COMP-3.
      *
      *            EX-FELD SCHECK-NUMMER (ABSCHNR)
               20  TDS-EX-SCHECK-NR-FELD    PIC  9(013)    COMP-3.
      *
      *            USERID DES ERFASSERS
               20  TDS-ERST-ERFASSER
                   REDEFINES TDS-EX-SCHECK-NR-FELD PIC X(007).
      *
      *            EX-FELD-SCHECK-BETRAG (ABSCHBET)
               20  TDS-EX-SCHECK-BETRAG-FELD PIC S9(005)V99 COMP-3.
      *
               20  FILLER   REDEFINES TDS-EX-SCHECK-BETRAG-FELD.
      *            LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
      *            (PRODUKT 7)
                   30  TDS-BEITRAG-EFF-7    PIC S9(007)    COMP-3.
      *
               20  TDS-EIGEN-ANTRAG REDEFINES
                   TDS-EX-SCHECK-BETRAG-FELD.
      *            EIGENANTRAG J/N
150807*            30 TDS-EIGEN-ANTRAG-KZ   PIC  X(001).
150807             30 FILLER                PIC  X(001).
                   30 TDS-EIGEN-A-FILLER    PIC  X(003).
      *
               20  TDS-BLTZ                 PIC  9(009)    COMP-3.
      *
               20  FILLER                 REDEFINES TDS-BLTZ.
                   30  FILLER               PIC  X(001).
      *            LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
      *            (PRODUKT 8)
                   30  TDS-BEITRAG-EFF-8    PIC S9(007)    COMP-3.
      *
               20  FILLER                   REDEFINES TDS-BLTZ.
      *            JAHRESNETTOPRÄMIE FÜR GENERALI LEBEN (GES.26)
      *            BEI PGEB-GES=20 (AML) -> BEITRAGZAHLUNGSD. IN MM
                   30  TDS-JNP              PIC S9(007)V99 COMP-3.
      *
               20  FILLER                   REDEFINES TDS-BLTZ.
      *            PRÄMIESUMME FÜR WA-LEBEN (GES.25)
                   30  TDS-PRAEMSUM-GES25   PIC S9(007)V99 COMP-3.
      *
      *            EX-FELD (ABKTONR)
               20  TDS-EX-FELD-KONTO-NR     PIC  9(011)    COMP-3.
      *
               20  FILLER              REDEFINES
                   TDS-EX-FELD-KONTO-NR.
                   30  FILLER               PIC  X(002).
      *            LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
      *            (PRODUKT 9)
                   30  TDS-BEITRAG-EFF-9    PIC S9(007)    COMP-3.
      *
      *            SUMME BAWAG(GES. 85) BIS ZU 18.000.000 Ö
               20  FILLER   REDEFINES TDS-EX-FELD-KONTO-NR.
                   30 FILLER                PIC X(001).
                   30 TDS-BAWAG-SUMME       PIC S9(009)    COMP-3.
      *
               20  FILLER    REDEFINES TDS-EX-FELD-KONTO-NR.
      *            STELLE 11-15 BEI TARIFEN
                   30 TDS-TARIF11-15        PIC  X(005).
                   30 FILLER                PIC  X(001).
      *
      *            FELD (ABANDATUM)
               20  TDS-AN-AUFN-DAT          PIC S9(007)    COMP-3.
      *
      *            FELD (ABANERFDAT) = STRUKTURDATUM
      *            AUCH BEZEICHNET ALS ANTRAGS-ERFASSUNGS-DATUM
               20  TDS-STRUK-DAT            PIC  9(007)    COMP-3.
      *
               20  TDS-ANTR-STATUS-1        PIC  9(002).
      *
               20  TDS-ANTR-STATUS-2        PIC  9(002).
      *
               20  TDS-ANTR-STATUS-3        PIC  9(002).
      *
               20  TDS-STATUS-DAT-1         PIC  9(007)    COMP-3.
      *
               20  TDS-STATUS-DAT-2         PIC  9(007)    COMP-3.
      *
               20  TDS-STATUS-DAT-3         PIC  9(007)    COMP-3.
      *
               20  TDS-ANTRAGS-ART          PIC  X(002).
      *
               20  TDS-DIR                  PIC  9(003).
      *
               20  TDS-VKBZ                 PIC  9(001).
100407         20  TDS-BERATERPROTOKOLL REDEFINES
100407             TDS-VKBZ                 PIC  X(001).
      *
      *            FELD(ABDTADAT)(DATUM STRUKTUR ZU AML ÜBERTRAGEN)
               20  TDS-DTA-STRUK-DAT        PIC  9(007)    COMP-3.
      *
               20  TDS-AG8-AENDERUNG        PIC  X(001).
      *
               20  TDS-ANTRAG-STRUKTUR      OCCURS 18.
                   30  TDS-MA-ST-NR         PIC  9(007)    COMP-3.
                   30  TDS-PRZ              PIC  X(001).
                   30  TDS-KNR-EMP-TYP      PIC  9(003)    COMP-3.
                   30  TDS-EMP-STUFE        PIC  X(004).
                   30  TDS-DIFF-PROV-STUFE  PIC  9(004)V9  COMP-3.
      *
      *            (PIN=INTERNE VERTRAGSNUMMER)NUR BEI GES. DIE MIT
      *             PIN ARBEITEN GEFÜLLT)
               20  TDS-PIN                  PIC  9(007)    COMP-3.
      *
      *            BEI PGEB-GES=20 (AML) -> HV-DAUER IN MM
               20  TDS-KRED                 PIC  9(007)V99 COMP-3.
      *
               20  TDS-PROV                 PIC  9(005)V99 COMP-3.
      *
               20  TDS-ABR-KZ               OCCURS 6.
                   30  TDS-AB-KZ            PIC  X(001).
      *
               20  TDS-ABFAELLIG1           PIC  9(007)    COMP-3.
      *
               20  TDS-EA40X                PIC  X(001).
      *
               20  TDS-BUZDAUER             PIC  9(002).
      *
               20  TDS-BAVKZ                PIC  X(001).
      *
               20  TDS-FOLGE                PIC  X(001).
      *
               20  TDS-ABFAELLIG2           PIC  9(007)    COMP-3.
      *
               20  TDS-ABFAELLIG3           PIC  9(007)    COMP-3.
      *
               20  TDS-ABFAELLIG4           PIC  9(007)    COMP-3.
      *
               20  TDS-ABFAELLIG5           PIC  9(007)    COMP-3.
      *
               20  TDS-ABFAELLIG6           PIC  9(007)    COMP-3.
      *
               20  TDS-HISKZ                PIC  X(001).
      *
               20  TDS-VZAN                 PIC  X(001).
      *
               20  TDS-GEB-DAT              PIC  9(007)    COMP-3.
      *
      *            BEI AGES=38 (DWS) -> DYNAMIK IN %
      *            BEI AGES=39 (GEN-INV) -> DYNAMIK IN %
      *            BEI AGES=40 (DIT) -> DYNAMIK IN %
      *            BEI AGES=48 (DWS) -> DYNAMIK IN %
               20  TDS-DYNAMIK REDEFINES TDS-GEB-DAT
                                         PIC S9(005)V9(002) COMP-3.
      *
               20  TDS-VERSDAUER-HV         PIC  9(002).
      *
               20  TDS-EINALTER1            PIC  9(002).
      *
               20  TDS-EINALTER2            PIC  9(002).
      *
               20  TDS-AMV-VVNR-KZ          PIC  X(001).
                   88  TDS-AMV-VVNR-DEFINIERT    VALUE '1', '2'.
                   88  TDS-EFFEKTIV-INIT         VALUE '1'.
                   88  TDS-VVNR-VORHANDEN        VALUE '2'.
      *
               20  TDS-STATISTIK-DAT        PIC S9(007)    COMP-3.
      *
               20  TDS-STATISTIK-DAT-X REDEFINES TDS-STATISTIK-DAT
                                            PIC  X(004).
      *
      *        IN DIESER WÄHRUNG (KTY=47) IST DER ANTRAG GESPEICHERT
               20  TDS-WAEHRUNGSTYP-SPEICHERUNG PIC  X(001).
      *
               20  TDS-WAEHRUNGSTYP-SPEICHERUNG-9 REDEFINES
                   TDS-WAEHRUNGSTYP-SPEICHERUNG PIC  9(001).
      *
      *        IN DIESER WÄHRUNG WURDE DER ANTRAG EINGEGEBEN
      *        (=BENUTZERSCHICHT)
               20  TDS-WAEHRUNGSTYP-EINGABE     PIC  X(001).
      *
               20  TDS-WAEHRUNGSTYP-EINGABE-9 REDEFINES
                   TDS-WAEHRUNGSTYP-EINGABE     PIC  9(001).
      *
      *        FELDER FÜR WA-LEBEN (GES.25) DIE ANZEIGEN OB EIN
      *        LV- VERTRAG ZUSÄTZLICH BERUFSUNFÄHIGKEIT (BUZR/BUZB)
      *        UNFALL-TOD/INVALIDITÄTS (UTZ) DD-ZUSATZVERS. ENTHÄLT
150807*        20  TDS-BUZR-GES25               PIC  X(001).
150807         20  TDS-EIGEN-ANTRAG-KZ          PIC  X(001).
               20  TDS-BUZB-GES25               PIC  X(001).
               20  TDS-UTZ-GES25                PIC  X(001).
               20  TDS-DDZ-GES25                PIC  X(001).
               20  FILLER                       PIC  X(002).
