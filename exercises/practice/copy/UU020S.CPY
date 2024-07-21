      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU020S.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/UU020S.CPv  $
      *
      *   Rev 2.14   Jul 15 2005 11:45:44   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.13   Jul 15 2005 10:18:52   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.12   Jul 01 2005 13:23:54   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.11   Jun 03 2005 15:47:42   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.10   Jan 13 2005 10:34:50   t13ae34
      *AAN00305: 2005/01: DWS (48) Sparpläne
      *
      *   Rev 2.9   Dec 22 2004 13:42:42   t13ae34
      *AAN00305: 2005/01: DWS (48) Sparpläne
      *
      *   Rev 2.8   Oct 27 2004 09:33:30   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.7   Oct 22 2004 15:43:50   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.6   Sep 05 2003 14:56:16   t13ae34
      *AAN00434 - Eigenantrag
      *
      *   Rev 2.5   05 Jun 2001 15:15:16   t13ae23
      *AAN00298 - Neu Ges. 85 BAWAG -
      *
      *   Rev 2.4   22 Sep 2000 19:30:12   T13AE34
      *AAN00280-Antrag Optimierung
      *
      *   Rev 2.3   18 Jul 2000 13:08:44   T13AE34
      *AAN00243-AML-BUZ % 3-stellig
      *
      *   Rev 2.2   10 Jul 2000 13:53:16   T13AE23
      *AAN00252 - Neue Felder für WA-L - 
      *
      *   Rev 2.1   Sep 23 1999 09:35:12   T13AE34
      *AAN00234-Generali Leben Ges. 26
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   07 Aug 1998 11:45:24   T13AE34
      *AAN00167 - AAN00166 Euro im Antragsbereich
      *
      *   Rev 1.2   29 Apr 1998 09:23:14   T13AE34
      *AAN00165 - Statistikdatum
      *
      *   Rev 1.1   Dec 01 1995 17:02:08   T13AE17
      *AFAN0091 Kükenhöhner neue Tarife AM-L
      *
      *   Rev 1.0   Nov 04 1995 09:46:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER ANTRAGS-DATEI
      *    ------------------------------
      *
      *    LAENGE = 660 BYTES
      *
      *---------------------------------------------------------------
      *  AENDERUNG                               NAME         DATUM
      *---------------------------------------------------------------
      *  NEU                                     JACOBSEN (GEDO)
      *                                          SCHORN (AGS) 20.11.79
      *  AHISHZ  HISTORIENSATZ-KZ NEU            RIBBERT         05.84
      *  AKBDAT VON 9(6) C-3 IN 9(7) C-3         RIBBERT         08.84
      *  AKBDAT UMGENANNT IN AIVSNR              RIBBERT         08.85
      *  ASPBHBK F.GES 61 EINGERICHTET           GRUENING        02.86
      *  ASPDREBA F. GES 62 EINGERICHTET         GRUENING        02.86
      *  ABETREUER AUF 18 ERWEITERT, SATZLAENGE 629 BYTE
      *                                          GARRE           01.88
      *  BEZEICHNUNGEN GEAENDERT WEGEN NEUER PRODUKTE AML
      *                                          GARRE           12.88
      *  AWBKZ1 DURCH ABAVKZ ERSETZT             MUELLER      14.12.90
      *  SCHECK-FELDER NEU                       MUELLER      11.06.91
      *  A2NPROZ DURCH AVKBZ ERSETZT             MUELLER      18.02.93
      *  SATZLAENGE GEAENDERT AUF 660 BYTE WG. NEUER PLZ UND AUSL-KZ
      *                                          MUELLER      25.06.93
      *  AFAN0004 - AVZAN ERSETZT AWBKZ          MUELLER      30.09.93
      *  AFAN0016 - NEUES FELD AGEBDAT           MUELLER      28.03.94
      *  AFAN0091 - NEUE FELDER AVERSDAUER-HV, AEINALTER1, AEINALTER2
      *                                          KUEKENHOEHNER 1.12.95
      *  AAN00167 - NEUE FELDER: AWAEHRUNG-SUMME-
      *                          ABEITR-EFF-
      *                          AAMV-VVNR-KZ
      *  AAN00165                ASTATISTIK-DATUM             16.02.98
      *  AAN00166   ENTFERNT: AWAEHRUNG-SUMME-..              23.06.98
      *             NEU     : AWAEHRUNGSTYP-SPEICHERUNG
      *                       AWAEHRUNGSTYP-EINGABE
      *---------------------------------------------------------------
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
      * AAN00298!05.06.01!2.5  ! FELD BEARBEITUNGSGEBÜR U.!          *
      *         !        !     ! BAWAG-SUMME FÜR NEUE GES !          *
      *         !        !     ! 85                       ! RAHNER   *
      * AAN00434!29.08.03!2.6  ! EIGENANTRAG              ! HEIDER   *
      * AAN00531!22.10.04!2.7  ! AML NEUE TARIFE NACH AEG ! HEIDER   *
      * AAN00531!22.10.04!2.8  ! AML NEUE TARIFE NACH AEG ! HEIDER   *
      * AAN00536!15.12.04!2.9  ! DWS SPARPLÄNE            ! HEIDER   *
      * AAN00536!03.01.05!2.10 ! DWS SPARPLÄNE            ! HEIDER   *
      * AAN00582!30.06.05!2.11 ! DIT SPARPLÄNE            ! RAHNER   *
      * AAN00582!01.07.05!2.11 ! GEN-INVEST(39) SPARPLÄNE ! RAHNER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  UU020S.

      *----SATZART
           10  ASA                       PIC  9(002).

      *----KEY
           10  ANKEY.
               20  AMANR                 PIC  9(007)    COMP-3.
               20  AGES                  PIC  9(002).
               20  AVNR                  PIC  9(015)    COMP-3.

      *----BESTANDS-SATZ
           10  ASATZ.
               20  APRZMA                PIC  X(001).
               20  APRZVG                PIC  X(002).
               20  AMATCHCODE            PIC  X(008).
               20  FILLER                REDEFINES AMATCHCODE.
                   30  ACODE4            PIC  X(004).
                   30  FILLER            PIC  X(004).
               20  FILLER                REDEFINES AMATCHCODE.
                   30  ACODE5            PIC  X(005).
                   30  FILLER            PIC  X(003).
               20  FILLER                REDEFINES AMATCHCODE.
                   30  ACODE6            PIC  X(006).
                   30  FILLER            PIC  X(002).
               20  FILLER                REDEFINES AMATCHCODE.
                   30  ACODE7            PIC  X(007).
                   30  FILLER            PIC  X(001).
               20  AMANRVM               PIC  9(007)    COMP-3.
               20  APRZVM                PIC  X(001).
               20  APRODS.
                   30  APRODSCHL         PIC  X(002).
                   30  APRODSCHL2        PIC  X(002).
221004*                BEI AGES=20 (AML) -> PS 36 (37) ELEKTR. ANTRAG
                   30  APRODSCHL3        PIC  X(002).
221004*                BEI AGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
                   30  APRODSCHL4        PIC  X(002).
221004*                BEI AGES=20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
                   30  APRODSCHL5        PIC  X(002).
221004*                BEI AGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
                   30  APRODSCHL6        PIC  X(002).
                   30  APRODSCHL7        PIC  X(002).
                   30  APRODSCHL8        PIC  X(002).
                   30  APRODSCHL9        PIC  X(002).
               20  APROD                 REDEFINES APRODS.
                   30  APRS              PIC  X(002)           OCCURS 9.
               20  ATARIF                PIC  X(010).
030105*>>>>>
      *            BEI AGES=38 -> ZAHLWEISE JE PRODUKT
010705*            BEI AGES=39 -> ZAHLWEISE JE PRODUKT
300605*            BEI AGES=40 -> ZAHLWEISE JE PRODUKT
      *            BEI AGES=48 -> ZAHLWEISE JE PRODUKT
               20  AZAHLW-TABELLE        REDEFINES ATARIF.
                   30 AZAHLW-TAB         PIC  9(1)             OCCURS 9.
                   30 FILLER             PIC  X(1).
               20  AZAHLW-TABELLE-X      REDEFINES ATARIF.
                   30 AZAHLW-TAB-X       PIC  X(1)             OCCURS 9.
                   30 FILLER             PIC  X(1).
030105*<<<<<
230698*        20  FILLER                REDEFINES ATARIF.
230698*            30  AWAEHRUNG-SUMME-2 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-3 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-4 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-5 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-6 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-7 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-8 PIC  X(001).
230698*            30  AWAEHRUNG-SUMME-9 PIC  X(001).
230698*            30  FILLER            PIC  X(002).
221004*            BEI AGES=20 -> KENNZ. DIREKTVERSICHERUNG J/N
               20  AANREDE               PIC  X(001).
               20  AVNAME                PIC  X(026).
               20  AZUNAME               PIC  X(026).
               20  ASTR                  PIC  X(026).
160298         20  FILLER                REDEFINES ASTR.
221004*                BEI AGES=10 (AMV) -> EFFEKTIVER JAHRESBEITRAG
221004*                BEI AGES=20 (AML) -> BEITRÄGE ZUSATZVERSICHERUNG
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
221004*                ------------------------------------------------
221004*                BEI AGES=20 (AML) -> PENKA: ENDE HV-DAUER
160298             30  ABEITR-EFF-1      PIC S9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> PENKA: ENDE BZ-DAUER
160298             30  ABEITR-EFF-2      PIC S9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> PS 36+37 AUS ELEKTR. ANTRAG
160298             30  ABEITR-EFF-3      PIC S9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
160298             30  ABEITR-EFF-4      PIC S9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
160298             30  ABEITR-EFF-5      PIC S9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
160298             30  ABEITR-EFF-6      PIC S9(007)    COMP-3.
230698*            30  AWAEHRUNG-SUMME-1 PIC  X(001).
230698*            30  FILLER            PIC  X(001).
230698             30  FILLER            PIC  X(002).
               20  AAUSL                 PIC  X(003).
               20  APLZ                  PIC  X(009).
               20  AORT                  PIC  X(021).
               20  AEALTER               PIC  9(002).
               20  ADAUER                PIC  9(002).
               20  ABEGINN               PIC  9(007)    COMP-3.
               20  AABLAUF               PIC  9(007)    COMP-3.
               20  AZAHLW                PIC  9(001).
140700*        20  ABUZPROZ              PIC  9(002).
140700         20  ABUZPROZ              PIC  9(003)    COMP-3.
               20  AEINH.
                   30  AEREINH           PIC S9(006)V9  COMP-3.
                   30  AEREINH2          PIC S9(006)V9  COMP-3.
221004*                BEI AGES=20 (AML) -> PS 36+37 AUS ELEKTR. ANTRAG
                   30  AEREINH3          PIC S9(006)V9  COMP-3.
221004*                BEI AGES=20 (AML) -> PS 39 AUS ELEKTR. ANTRAG
                   30  AEREINH4          PIC S9(006)V9  COMP-3.
221004*                BEI AGES=20 (AML) -> PS 18 AUS ELEKTR. ANTRAG
                   30  AEREINH5          PIC S9(006)V9  COMP-3.
221004*                BEI AGES=20 (AML) -> PS 17 AUS ELEKTR. ANTRAG
                   30  AEREINH6          PIC S9(006)V9  COMP-3.
                   30  AEREINH7          PIC S9(006)V9  COMP-3.
                   30  AEREINH8          PIC S9(006)V9  COMP-3.
                   30  AEREINH9          PIC S9(006)V9  COMP-3.
               20  ABEINH                REDEFINES AEINH.
                   30  AABEIN            PIC S9(006)V9  COMP-3 OCCURS 9.
               20  AANSUMMEN.
                   30  AANSUMME          PIC S9(007)    COMP-3.
                   30  AANSUMME2         PIC S9(007)    COMP-3.
                   30  AANSUMME3         PIC S9(007)    COMP-3.
                   30  AANSUMME4         PIC S9(007)    COMP-3.
                   30  AANSUMME5         PIC S9(007)    COMP-3.
                   30  AANSUMME6         PIC S9(007)    COMP-3.
                   30  AANSUMME7         PIC S9(007)    COMP-3.
                   30  AANSUMME8         PIC S9(007)    COMP-3.
                   30  AANSUMME9         PIC S9(007)    COMP-3.
               20  AANS                  REDEFINES AANSUMMEN.
                   30  AANSUM            PIC S9(007)    COMP-3 OCCURS 9.
               20  AANBEITRAG            PIC S9(007)V99 COMP-3.
               20  AAPEINH               PIC S9(006)V9  COMP-3.
               20  AAPSUMME              PIC S9(007)    COMP-3.
               20  AAPBEITRAG            PIC S9(005)V99 COMP-3.
050601*            BEARBEITUNGSGEBÜR GES. 85 (BAWAG)
050601         20  AANBEARBEITGEB REDEFINES AAPBEITRAG
050601                                   PIC S9(5)V99   COMP-3.
               20  ASCHNR                PIC  9(013)    COMP-3.
140900*            USERID DES ERFASSERS
140900         20  AERFID REDEFINES ASCHNR PIC  X(007).
               20  ASCHBET               PIC S9(005)V99 COMP-3.
160298         20  FILLER                REDEFINES ASCHBET.
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABEITR-EFF-7      PIC S9(007)    COMP-3.
               20  ASCHECK-FELDER-NEU    REDEFINES ASCHBET.
290803*            EIGENANTRAG J/N
                   30  ASCHECK           PIC  X(001).
                   30  ASCHFILLER        PIC  X(003).
               20  ABLTZ                 PIC  9(009)    COMP-3.
160298         20  FILLER                REDEFINES ABLTZ.
160298             30  FILLER            PIC  X(001).
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABEITR-EFF-8      PIC S9(007)    COMP-3.
220999         20  FILLER                REDEFINES ABLTZ.
220999*                JAHRESNETTOPRÄMIE FÜR GENERALI LEBEN (GES.26)
221004*                BEI AGES=20 (AML) -> BEITRAGSZAHLHUNGSDAUER IN MM
220999             30  AJNP              PIC S9(007)V99 COMP-3.
300600         20  FILLER                REDEFINES ABLTZ.
300600*                PRÄMIESUMME FÜR WA-LEBEN (GES.25)
300600             30  APRAEMSUM         PIC S9(007)V99 COMP-3.
               20  AKTONR                PIC  9(011)    COMP-3.
160298         20  FILLER                REDEFINES AKTONR.
160298             30  FILLER            PIC  X(002).
151204*                LAUFZEITEN IN MONATEN, WENN LAUFZEITABHÄNGIG
160298             30  ABEITR-EFF-9      PIC S9(007)    COMP-3.
050601*                SUMME BAWAG BIS ZU 18.000.000 Ö
050601         20  FILLER                REDEFINES AKTONR.
050601             30 FILLER             PIC  X(001).
050601             30 AABAWAG-SUMME      PIC S9(009)    COMP-3.
221004*>>>>>
               20  FILLER                REDEFINES AKTONR.
      *               STELLE 11-15 BEI TARIFEN
                   30 ATARIF11-15        PIC  X(005).
050601             30 FILLER             PIC  X(001).
221004*<<<<<
               20  AANDATUM              PIC S9(007)    COMP-3.
               20  AANERFDAT             PIC  9(007)    COMP-3.
               20  AANSTATUS1            PIC  9(002).
               20  AANSTATUS2            PIC  9(002).
               20  AANSTATUS3            PIC  9(002).
               20  ASTATDAT1             PIC  9(007)    COMP-3.
               20  ASTATDAT2             PIC  9(007)    COMP-3.
               20  ASTATDAT3             PIC  9(007)    COMP-3.
               20  AANART                PIC  X(002).
               20  ADIREKTION            PIC  9(003).
               20  AVKBZ                 PIC  9(001).
               20  ADTADAT               PIC  9(007)    COMP-3.
               20  AAG8-AEND             PIC  X(001).
               20  ABETREUER                                  OCCURS 18.
                   30  AGMANR            PIC  9(007)    COMP-3.
                   30  AGPRZ             PIC  X(001).
                   30  AGSTRSTUFE        PIC  9(003)    COMP-3.
                   30  AGSTRTEXT         PIC  X(004).
                   30  AGPROZ            PIC  9(004)V9  COMP-3.
               20  AIVSNR                PIC  9(007)    COMP-3.
221004*                BEI AGES=20 (AML) -> HV-DAUER IN MM
               20  AKRED                 PIC  9(007)V9  COMP-3.
               20  APROV                 PIC  9(005)V9  COMP-3.
               20  AABR-KZ                                     OCCURS 6.
                   30  AAB-KZ            PIC  X(001).
               20  AABFAELLIG1           PIC S9(007)    COMP-3.
               20  AEA40X                PIC  X(001).
               20  ABUZDAUER             PIC  9(002).
               20  ABAVKZ                PIC  X(001).
               20  AAFOLGE               PIC  X(001).
               20  AABFAELLIG2           PIC S9(007)    COMP-3.
               20  AABFAELLIG3           PIC S9(007)    COMP-3.
               20  AABFAELLIG4           PIC S9(007)    COMP-3.
               20  AABFAELLIG5           PIC S9(007)    COMP-3.
               20  AABFAELLIG6           PIC S9(007)    COMP-3.
               20  AHISKZ                PIC  X(001).
               20  AVZAN                 PIC  X(001).
               20  AGEBDAT               PIC S9(007)    COMP-3.
030105*                BEI AGES=38 (DWS) -> DYNAMIK IN %
010705*                BEI AGES=39 (GEN-INV) -> DYNAMIK IN %
300605*                BEI AGES=40 (DIT) -> DYNAMIK IN %
030105*                BEI AGES=48 (DWS) -> DYNAMIK IN %
               20  ADYNAMIK REDEFINES AGEBDAT
                                         PIC S9(005)V9(002) COMP-3.
               20  AVERSDAUER-HV         PIC  9(002).
               20  AEINALTER1            PIC  9(002).
               20  AEINALTER2            PIC  9(002).
160298*        20  FILLER                PIC  X(013).
160298         20  AAMV-VVNR-KZ          PIC  X(001).
160298             88  AAMV-VVNR-DEFINIERT              VALUE '1', '2'.
160298             88  AEFFEKTIV-INIT                   VALUE '1'.
160298             88  AVVNR-VORHANDEN                  VALUE '2'.
160298         20  ASTATISTIK-DATUM      PIC S9(007)    COMP-3.
100398         20  ASTATISTIK-DATUM-X REDEFINES ASTATISTIK-DATUM
100398                                   PIC  X(004).
230698*        20  FILLER                PIC  X(008).
230698*            IN DIESER WäHRUNG (KTY=47) IST DER ANTRAG GESPEICHERT
230698         20  AWAEHRUNGSTYP-SPEICHERUNG  PIC  X(001).
230698         20  AWAEHRUNGSTYP-SPEICHERUNG-9 REDEFINES
230698             AWAEHRUNGSTYP-SPEICHERUNG  PIC  9(001).
230698*            IN DIESER WäHRUNG WURDE DER ANTRAG EINGEGEBEN
230698*            (=BENUTZERSCHICHT)
230698         20  AWAEHRUNGSTYP-EINGABE      PIC  X(001).
230698         20  AWAEHRUNGSTYP-EINGABE-9 REDEFINES
230698             AWAEHRUNGSTYP-EINGABE      PIC  9(001).
300600**** FELDER FÜR WA-LEBEN (GES.25) DIE ANZEIGEN OB EIN
300600**** LV- VERTRAG ZUSÄTZLICH BERUFSUNFÄHIGKEIT (BUZR/BUZB)
300600**** UNFALL-TOD/INVALIDITÄTS (UTZ) DD-ZUSATZVERS. ENTHÄLT
300600         20  ABUZR                      PIC  X(001).
300600         20  ABUZB                      PIC  X(001).
300600         20  AUTZ                       PIC  X(001).
300600         20  ADDZ                       PIC  X(001).
300600*        20  FILLER                     PIC  X(006).
300600         20  FILLER                     PIC  X(002).

      *----DUMMY-SATZ

           10  ADSATZ                    REDEFINES ASATZ.
               20  ADANZAHL              PIC S9(007)    COMP-3.
               20  ADLADEDAT             PIC  9(007)    COMP-3.
               20  ADELADEDAT            PIC  9(007)    COMP-3.
               20  ASPDIT                PIC  X(001).
               20  ASPIII                PIC  X(001).
               20  ASPBHI                PIC  X(001).
               20  ASPHYPO               PIC  X(001).
               20  ASPBHB                PIC  X(001).
               20  ASPDRB                PIC  X(001).
               20  FILLER                PIC  X(626).

