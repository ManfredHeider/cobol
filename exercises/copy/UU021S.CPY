      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU021S.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/UU021S.CPv  $
      *
      *   Rev 2.14   Jul 15 2005 11:45:52   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.13   Jul 15 2005 10:20:02   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.12   Jul 01 2005 13:24:04   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.11   Jun 03 2005 15:47:52   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.10   Jan 13 2005 10:37:48   t13ae34
      *AAN00305: 2005/01: DWS (48) Sparpläne
      *
      *   Rev 2.9   Dec 22 2004 13:42:52   t13ae34
      *AAN00305: 2005/01: DWS (48) Sparpläne
      *
      *   Rev 2.8   Oct 27 2004 09:36:28   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.7   Oct 22 2004 15:46:48   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.6   Sep 05 2003 15:01:20   t13ae34
      *AAN00434 - Eigenantrag
      *
      *   Rev 2.5   05 Jun 2001 15:15:34   t13ae23
      *AAN00298 - Neu Ges. 85 BAWAG -
      *
      *   Rev 2.4   22 Sep 2000 19:31:12   T13AE34
      *AAN00280-Antrag Optimierung
      *
      *   Rev 2.3   18 Jul 2000 13:08:46   T13AE34
      *AAN00243-AML-BUZ % 3-stellig
      *
      *   Rev 2.2   10 Jul 2000 13:53:36   T13AE23
      *AAN00252 - Neue Felder für WA-L -
      *
      *   Rev 2.1   Sep 23 1999 09:35:16   T13AE34
      *AAN00234-Generali Leben Ges. 26
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   07 Aug 1998 11:45:50   T13AE34
      *AAN00167 - AAN00166 Euro im Antragsbereich
      *
      *   Rev 1.2   29 Apr 1998 09:24:08   T13AE34
      *AAN00165 - Statistikdatum
      *
      *   Rev 1.1   Dec 01 1995 17:05:56   T13AE17
      *AFAN0091 Kükenhöhner neue Tarife AM-L
      *
      *   Rev 1.0   Nov 04 1995 09:47:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER  A N T R A G S - T D S
      *    ---------------------------------------
      *
      *    LAENGE = 682 BYTES
      *
      *-------------------------------------------------------------
      *          NEU
      *          21.01.80 JACOBSEN (GEDO), SCHORN (AGS)
      *-------------------------------------------------------------
      *          ABKBDAT UMBENANNT IN ABIVSNR
      *             08.85            RIBBERT
      *-------------------------------------------------------------
      *          ABBETREUER AUF 18 ERWEITERT, SATZLAENGE = 651 BYTE
      *             01.88            GARRE
      *-------------------------------------------------------------
      *          BEZEICHNUNGEN GEAENDERT WEGEN NEUER PRODUKTE AML
      *             12.88            GARRE
      *-------------------------------------------------------------
      *          ABWBKZ1 DURCH ABBAVKZ ERSETZT
      *          14.12.90            MUELLER
      *-------------------------------------------------------------
      *          SCHECK-FELDER NEU
      *          11.06.91            MUELLER
      *-------------------------------------------------------------
      *          AB2NPROZ DURCH ABVKBZ ERSETZT
      *          18.02.93            MUELLER
      *-------------------------------------------------------------
      *          ABPLZ ERWEITERT, ABAUSL EINGEF., SATZLAENGE = 682 BYTE
      *          25.06.93            MUELLER
      *-------------------------------------------------------------
      * AFAN0004 ABVZAN ERSETZT ABWBKZ
      *          30.09.93            MUELLER
      *-------------------------------------------------------------
      * AFAN0016 NEUES FELD ABGEBDAT
      *          28.03.94            MUELLER
      *-------------------------------------------------------------
      * AFRZ0012 UMSTELLUNG KUS / RACF
      *          26.08.94            WIECHERS
      *-------------------------------------------------------------
      * AFAN0091 NEUE FELDER ABVERSDAUER-HV, ABEINALTER1, ABEINALTER2
      *          01.12.95            KUEKENHOEHNER
      *-------------------------------------------------------------
      * AAN00167 NEUE FELDER ABWAEHRUNG-SUMME-
      *                      ABBEITR-EFF
      *                      ABAMV-VVNR-KZ
      * AAN00165             ABSTATISTIK-DATUM
      *          16.02.98            KUEKENHOEHNER
      *  AAN00166   ENTFERNT: AWAEHRUNG-SUMME-..              23.06.98
      *             NEU     : AWAEHRUNGSTYP-SPEICHERUNG
      *                       AWAEHRUNGSTYP-EINGABE
      *----------------------------------------------------------------
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00234!22.09.99!2.1  ! NEU                      ! HEIDER   *
      * AAN00252!30.06.00!2.2  ! NEUE FELDER BUZR,BUZB,   !          *
      *         !        !     ! UTZ,DDZ,PRÄMIESUMME FÜR  !          *
      *         !        !     ! WA-L GES.25              ! RAHNER   *
      * AAN00243!14.07.00!2.3  ! AML-BUZ % 3-STELLIG      ! HEIDER   *
      * AAN00280!14.09.00!2.4  ! ERFASSER-USER-ID         ! HEIDER   *
      * AAN00298!05.06.01!2.5  ! NEUE FELDER BEARBGEBÜR U.!          *
      *         !        !     ! SUMME-BAWAG FÜR GES 85   ! RAHNER   *
      * AAN00434!29.08.03!2.6  ! EIGENANTRAG              ! HEIDER   *
      * AAN00531!22.10.04!2.7  ! AML NEUE TARIFE NACH AEG ! HEIDER   *
      * AAN00531!03.01.05!2.8  ! AML NEUE TARIFE NACH AEG ! HEIDER   *
      * AAN00536!03.01.05!2.9  ! DWS SPARPLÄNE            ! HEIDER   *
      * AAN00536!03.01.05!2.10 ! DWS SPARPLÄNE            ! HEIDER   *
      * AAN00582!30.06.05!2.11 ! DIT SPARPLÄNE            ! RAHNER   *
      * AAN00582!01.07.05!2.11 ! GEN-INV(39) SPARPLÄNE    ! RAHNER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  UU021S.

      *----AN-TDS-SPEZIFISCHER TEIL (22 BYTE)

           10  ABMODUS                    PIC  X(002).
           10  ABDATUM                    PIC  9(007)    COMP-3.
           10  ABUHR                      PIC  9(009)    COMP-3.
           10  ABTERM                     PIC  X(004).
           10  ABUSERID                   PIC  X(007).

      *----AB HIER ABBILD DES ANTRAGSSATZES

           10  ABSA                       PIC  9(002).
           10  ABKEY.
               20  ABMANR                 PIC  9(007)    COMP-3.
               20  ABGES                  PIC  9(002).
               20  ABVNR                  PIC  9(015)    COMP-3.

           10  ABSATZ.
               20  ABPRZMA                PIC  X(001).
               20  ABPRZVG                PIC  X(002).
               20  ABMATCHCODE            PIC  X(008).
               20  ABMANRVM               PIC  9(007)    COMP-3.
               20  ABPRZVM                PIC  X(001).
               20  ABPRODSCHL             PIC  X(002).
               20  ABPRODSCHL2            PIC  X(002).
221004*                BEI ABGES=20 (AML) -> PS 36+37 AUS ELEKTR. ANTRAG
               20  ABPRODSCHL3            PIC  X(002).
221004*                BEI ABGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
               20  ABPRODSCHL4            PIC  X(002).
221004*                BEI ABGES=20 (AML) -> PS 38 AUS ELEKTR. ANTRAG
               20  ABPRODSCHL5            PIC  X(002).
221004*                BEI ABGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
               20  ABPRODSCHL6            PIC  X(002).
               20  ABPRODSCHL7            PIC  X(002).
               20  ABPRODSCHL8            PIC  X(002).
               20  ABPRODSCHL9            PIC  X(002).
               20  ABTARIF                PIC  X(010).
030105*>>>>>
      *            BEI AGES=38 -> ZAHLWEISE JE PRODUKT
010705*            BEI AGES=39 -> ZAHLWEISE JE PRODUKT
300605*            BEI AGES=40 -> ZAHLWEISE JE PRODUKT
      *            BEI AGES=48 -> ZAHLWEISE JE PRODUKT
               20  ABZAHLW-TABELLE        REDEFINES ABTARIF.
                   30 ABZAHLW-TAB         PIC  9(1)            OCCURS 9.
                   30 FILLER              PIC  X(1).
               20  AZAHLW-TABELLE-X       REDEFINES ABTARIF.
                   30 ABZAHLW-TAB-X       PIC  X(1)            OCCURS 9.
                   30 FILLER              PIC  X(1).
030105*<<<<<
230698*        20  FILLER                 REDEFINES ABTARIF.
230698*            30  ABWAEHRUNG-SUMME-2 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-3 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-4 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-5 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-6 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-7 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-8 PIC  X(001).
230698*            30  ABWAEHRUNG-SUMME-9 PIC  X(001).
230698*            30  FILLER             PIC  X(002).
221004*            WENN ABGES=20 -> KENNZ. DIREKTVERSICHERUNG J/N
               20  ABANREDE               PIC  X(001).
               20  ABVNAME                PIC  X(026).
               20  ABZUNAME               PIC  X(026).
               20  ABSTR                  PIC  X(026).
160298         20  FILLER                 REDEFINES ABSTR.
221004*                BEI ABGES=10 (AMV) -> EFFEKTIVER JAHRESBEITRAG
221004*                BEI ABGES=20 (AML) -> BEITRÄGE ZUSATZVERSICHERUNG
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
221004*                ------------------------------------------------
221004*                BEI ABGES=20 (AML) -> PENKA: ENDE HV-DAUER
160298             30  ABBEITR-EFF-1      PIC S9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> PENKA: ENDE BZ-DAUER
160298             30  ABBEITR-EFF-2      PIC S9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 36+37 AUS ELEKTR. ANTRAG
160298             30  ABBEITR-EFF-3      PIC S9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
160298             30  ABBEITR-EFF-4      PIC S9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
160298             30  ABBEITR-EFF-5      PIC S9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
160298             30  ABBEITR-EFF-6      PIC S9(007)    COMP-3.
230698*            30  ABWAEHRUNG-SUMME-1 PIC  X(001).
230698*            30  FILLER             PIC  X(001).
230698             30  FILLER             PIC  X(002).
               20  ABAUSL                 PIC  X(003).
               20  ABPLZ                  PIC  X(009).
               20  ABORT                  PIC  X(021).
               20  ABEALTER               PIC  9(002).
               20  ABDAUER                PIC  9(002).
               20  ABBEGINN               PIC  9(007)    COMP-3.
               20  ABABLAUF               PIC  9(007)    COMP-3.
               20  ABZAHLW                PIC  9(001).
140700*        20  ABBUZPROZ              PIC  9(002).
140700         20  ABBUZPROZ              PIC  9(002)    COMP-3.
               20  ABEREINH               PIC S9(006)V9  COMP-3.
               20  ABEREINH2              PIC S9(006)V9  COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 36+37 AUS ELEKTR. ANTRAG
               20  ABEREINH3              PIC S9(006)V9  COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
               20  ABEREINH4              PIC S9(006)V9  COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
               20  ABEREINH5              PIC S9(006)V9  COMP-3.
221004*                BEI ABGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
               20  ABEREINH6              PIC S9(006)V9  COMP-3.
               20  ABEREINH7              PIC S9(006)V9  COMP-3.
               20  ABEREINH8              PIC S9(006)V9  COMP-3.
               20  ABEREINH9              PIC S9(006)V9  COMP-3.
               20  ABANSUMME              PIC S9(007)    COMP-3.
               20  ABANSUMME2             PIC S9(007)    COMP-3.
               20  ABANSUMME3             PIC S9(007)    COMP-3.
               20  ABANSUMME4             PIC S9(007)    COMP-3.
               20  ABANSUMME5             PIC S9(007)    COMP-3.
               20  ABANSUMME6             PIC S9(007)    COMP-3.
               20  ABANSUMME7             PIC S9(007)    COMP-3.
               20  ABANSUMME8             PIC S9(007)    COMP-3.
               20  ABANSUMME9             PIC S9(007)    COMP-3.
               20  ABANBEITRAG            PIC S9(007)V99 COMP-3.
               20  ABAPEINH               PIC S9(006)V9  COMP-3.
               20  ABAPSUMME              PIC S9(007)    COMP-3.
               20  ABAPBEITRAG            PIC S9(005)V99 COMP-3.
050601*            BEARBEITUNGSGEBÜR GES. 85 (BAWAG)
050601         20  ABANBEARBEITGEB REDEFINES ABAPBEITRAG
050601                                    PIC S9(5)V99   COMP-3.
               20  ABSCHNR                PIC  9(013)    COMP-3.
140900*            USERID DES ERFASSERS
140900         20  ABERFID REDEFINES ABSCHNR PIC  X(007).
               20  ABSCHBET               PIC S9(005)V99 COMP-3.
160298         20  FILLER                 REDEFINES ABSCHBET.
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABBEITR-EFF-7      PIC S9(007)    COMP-3.
               20  ABSCHECK-FELDER-NEU    REDEFINES ABSCHBET.
290803*            EIGENANTRAG J/N
                   30 ABSCHECK            PIC  X(001).
                   30 ABSCHFILLER         PIC  X(003).
               20  ABBLTZ                 PIC  9(009)    COMP-3.
160298         20  FILLER                 REDEFINES ABBLTZ.
160298             30  FILLER             PIC  X(001).
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABBEITR-EFF-8      PIC S9(007)    COMP-3.
220999         20  FILLER                REDEFINES ABBLTZ.
220999*                JAHRESNETTOPRÄMIE FÜR GENERALI LEBEN (GES.26)
221004*                BEI ABGES=20 (AML) -> BEITRAGZAHLUNGSDAUER IN MM
220999             30  ABJNP              PIC S9(007)V99 COMP-3.
300600         20  FILLER                REDEFINES ABBLTZ.
300600*                PRÄMIESUMME FÜR WA-LEBEN (GES.25)
300600             30  ABPRAEMSUM         PIC S9(007)V99 COMP-3.
               20  ABKTONR                PIC  9(011)    COMP-3.
160298         20  FILLER                 REDEFINES ABKTONR.
160298             30  FILLER             PIC  X(002).
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABBEITR-EFF-9      PIC S9(007)    COMP-3.
050601*                SUMME BAWAG BIS ZU 18.000.000 Ö
050601         20  FILLER                REDEFINES ABKTONR.
050601             30 FILLER              PIC  X(001).
050601             30 ABBAWAG-SUMME       PIC S9(009)    COMP-3.
221004*>>>>>
               20  FILLER                REDEFINES ABKTONR.
      *               STELLE 11-15 BEI TARIFEN
                   30 ABTARIF11-15        PIC  X(005).
                   30 FILLER              PIC  X(001).
221004*<<<<<
               20  ABANDATUM              PIC S9(007)    COMP-3.
               20  ABANERFDAT             PIC  9(007)    COMP-3.
               20  ABANSTATUS1            PIC  9(002).
               20  ABANSTATUS2            PIC  9(002).
               20  ABANSTATUS3            PIC  9(002).
               20  ABSTATDAT1             PIC  9(007)    COMP-3.
               20  ABSTATDAT2             PIC  9(007)    COMP-3.
               20  ABSTATDAT3             PIC  9(007)    COMP-3.
               20  ABANART                PIC  X(002).
               20  ABDIREKTION            PIC  9(003).
               20  ABVKBZ                 PIC  9(001).
               20  ABDTADAT               PIC  9(007)    COMP-3.
               20  ABAG8-AEND             PIC  X(001).
               20  ABBETREUER                                 OCCURS 18.
                   30  ABBMANR            PIC  9(007)    COMP-3.
                   30  ABBPRZ             PIC  X(001).
                   30  ABBSTRSTUFE        PIC  9(003)    COMP-3.
                   30  ABBSTRTEXT         PIC  X(004).
                   30  ABBPROZ            PIC  9(004)V9  COMP-3.
               20  ABIVSNR                PIC  9(007)    COMP-3.
221004*                BEI ABGES=20 (AML) -> HV-DAUER IN MM
               20  ABKRED                 PIC  9(007)V99 COMP-3.
               20  ABPROV                 PIC  9(005)V99 COMP-3.
               20  ABABR-KZ                                    OCCURS 6.
                   30  ABAB-KZ            PIC  X(001).
               20  ABABFAELLIG1           PIC  9(007)    COMP-3.
               20  ABEA40X                PIC  X(001).
               20  ABBUZDAUER             PIC  9(002).
               20  ABBAVKZ                PIC  X(001).
               20  ABFOLGE                PIC  X(001).
               20  ABABFAELLIG2           PIC  9(007)    COMP-3.
               20  ABABFAELLIG3           PIC  9(007)    COMP-3.
               20  ABABFAELLIG4           PIC  9(007)    COMP-3.
               20  ABABFAELLIG5           PIC  9(007)    COMP-3.
               20  ABABFAELLIG6           PIC  9(007)    COMP-3.
               20  ABHISKZ                PIC  X(001).
               20  ABVZAN                 PIC  X(001).
               20  ABGEBDAT               PIC  9(007)    COMP-3.
030105*                BEI AGES=38 (DWS) -> DYNAMIK IN %
010705*                BEI AGES=39 (GEN-INV) -> DYNAMIK IN %
300605*                BEI AGES=40 (DIT) -> DYNAMIK IN %
030105*                BEI AGES=48 (DWS) -> DYNAMIK IN %
               20  ABDYNAMIK REDEFINES ABGEBDAT
                                          PIC S9(005)V9(002) COMP-3.
               20  ABVERSDAUER-HV         PIC  9(002).
               20  ABEINALTER1            PIC  9(002).
               20  ABEINALTER2            PIC  9(002).
160298*        20  FILLER                 PIC  X(013).
160298         20  ABAMV-VVNR-KZ          PIC  X(001).
160298             88  ABAMV-VVNR-DEFINIERT              VALUE '1', '2'.
160298             88  ABEFFEKTIV-INIT                   VALUE '1'.
160298             88  ABVVNR-VORHANDEN                  VALUE '2'.
160298         20  ABSTATISTIK-DATUM      PIC S9(007)    COMP-3.
100398         20  ABSTATISTIK-DATUM-X REDEFINES ABSTATISTIK-DATUM
100398                                    PIC  X(004).
230698*        20  FILLER                 PIC  X(008).
230698*            IN DIESER WäHRUNG (KTY=47) IST DER ANTRAG GESPEICHERT
230698         20  ABWAEHRUNGSTYP-SPEICHERUNG PIC  X(001).
230698         20  ABWAEHRUNGSTYP-SPEICHERUNG-9 REDEFINES
230698             ABWAEHRUNGSTYP-SPEICHERUNG PIC  9(001).
230698*            IN DIESER WäHRUNG WURDE DER ANTRAG EINGEGEBEN
230698*            (=BENUTZERSCHICHT)
230698         20  ABWAEHRUNGSTYP-EINGABE     PIC  X(001).
230698         20  ABWAEHRUNGSTYP-EINGABE-9 REDEFINES
230698             ABWAEHRUNGSTYP-EINGABE     PIC  9(001).
300600**** FELDER FÜR WA-LEBEN (GES.25) DIE ANZEIGEN OB EIN
300600**** LV- VERTRAG ZUSÄTZLICH BERUFSUNFÄHIGKEIT (BUZR/BUZB)
300600**** UNFALL-TOD/INVALIDITÄTS (UTZ) DD-ZUSATZVERS. ENTHÄLT
300600         20  ABBUZR                     PIC  X(001).
300600         20  ABBUZB                     PIC  X(001).
300600         20  ABUTZ                      PIC  X(001).
300600         20  ABDDZ                      PIC  X(001).
300600*        20  FILLER                     PIC  X(006).
300600         20  FILLER                     PIC  X(002).


