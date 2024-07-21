      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPC0001.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPC0001.CPv  $
      *
      *   Rev 2.5   Oct 01 2003 13:58:30   t13ex27
      *FKI00780 - Falsche BA-Art von AML
      *
      *   Rev 2.4   Sep 29 2003 11:09:50   t13ex27
      *AKI00366 - Neue Besuchsauftrags-Art
      *
      *   Rev 2.3   12 Oct 2001 08:51:34   t13ex48
      *FKI00508
      *Fehler bei Waehrungs-KZ AML
      *
      *   Rev 2.2   Feb 21 2000 15:46:24   T13EX48
      *FKI00329
      *Änderung der Eingabe-Datei-Struktur
      *
      *   Rev 2.1   Jun 24 1999 17:55:26   T13EX21
      *APA00012 - PVCS-Kopf korrigiert
      *
      *   Rev 1.0   Nov 04 1995 08:02:34   P13SP13
      *Initial Revision.
      *
      *---------------------------------------------------------------*
      *   VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BA-DATEI          DRPC0001                 *
      *    -------------------------          ======                   *
      *                                                                *
      *    LAENGE = 700 BYTES        ERSTELLT  AM 25.05.87 / BIEHL     *
      *                              UMBENANNT AM 07.07.93 / BORK      *
      *                                                                *
      *    BA-DATEI FUER PC: INDEX-DATEI MIT INDEX = BA-SCHL           *
      *                      DATEINAME = U.UU0222                      *
      *                                                                *
      *    BA-DATEI FUER FFM: OHNE INDEX                               *
      *                       DATEINAME = U.UU0223 (UNSORTIERT)        *
      *                       DATEINAME = U.UU0224 (SORTIERT)          *
      *                       DATEINAME = U.UU0225 (SORTIERT)          *
      *                                                                *
      *----------------------------------------------------------------*
      * 05.02.1993  BORK                                               *
      * DATENFELDER FÜR AM-VERS IN DEN SATZ MIT AUFGENOMMEN UND        *
      * DEN SATZ AUF 700 BYTE ERWEITERT                                *
      *----------------------------------------------------------------*
      * 13.08.1993  BORK                                               *
      * NEUE FELDER AUF WUNSCH VON AM-LEBEN IN DEN SATZ AUFGENOMMEN.   *
      *----------------------------------------------------------------*
      * 18.08.93    BORK                                               *
      * FELDER DIE ERST SPÄTER GELIEFERT WERDEN AUSGESTERNT            *
      *----------------------------------------------------------------*
      * 28.02.94    BORK                                               *
      * NEUE BA-SPARTEN VON AM-VERS (UNFALL, HAFTPFLICHT, HAUSRAT UND  *
      * WOHNGEBÄUDE.                                                   *
      *----------------------------------------------------------------*
      * 19.07.95    BORK                                               *
      * NEUE REDEFINITION DES PIF-FELDES, BEI BA-ART = 6 WIRD HIER     *
      * VON DER AM-VERS DIE FÄLLIGKEIT EINGETRAGEN.                    *
      *----------------------------------------------------------------*
      * 18.02.2000  Sebbach                                            *
      * Strukturaenderung der Felder:                                  *
      *    - BA-VNPLZ  (Von PIC X(009) auf X(006) geaendert            *
      *    - BA-AGPLZ  (Von PIC X(009) auf X(006) geaendert            *
      *    - BA-VNORT  (Von PIC X(026) auf X(029) geaendert            *
      *    - BA-AGORT  (Von PIC X(026) auf X(029) geaendert            *
      * aufgrund einer Aenderung der AML-BA-Daten                      *
      *----------------------------------------------------------------*
210901* 21.09.2001  Sebbach                       FKI00508             *
      * Strukturaenderung des Feldes BA-PIF                            *
      *    hier ist im Feld BA-REST ein Waehrungs-KZ versteckt         *
      *    Dieses Feld wird durch eine saubere Definition ersetzt      *
      *----------------------------------------------------------------*
      * 12.09.2003 Frenzel                        AKI00366             *
      * Neue Felder für die BA-Art Ablehnung Dynamisierung             *
      *----------------------------------------------------------------*

       01  DRPC0001.
           05  BA-SCHL.
               07  BA-PCNR             PIC  9(005).
               07  BA-GES              PIC  9(002).
      * 10 = AM-VERS     20 = AM-LEBEN
               07  BA-VSNR             PIC  9(015).
               07  BA-ABSDAT           PIC  9(006).
               07  BA-GES-KZ           PIC  9(002).
      * SPARTEN-KZ:04= KFZ,05= UNFALL,06= HAUSRAT
      *            07= HAFTPFLICHT, 08= WOHNGEBÄUDE
      *            00 = SONSTIGE( ANDERE GESELLSCHAFTEN )
           05  BA-SATZ.
               07  BA-STATKZ               PIC  9(001).
               07  BA-STATDAT              PIC  9(006).
011003*        07  BA-ART                  PIC  9(001).
011003         07  BA-ART                  PIC  X(001).
               07  BA-VBNR                 PIC  9(007).
               07  BA-VBNAM                PIC  X(060).
               07  BA-PIF                  PIC  X(015).
               07  FILLER REDEFINES BA-PIF.
                 10  BA-DIR                PIC  X(003).
                 10  BA-FAELLIGKEIT        PIC  X(004).
210901*          10  BA-REST               PIC  X(008).
210901           10  FILLER                PIC  X(005).
210901           10  BA-WAEHR-KZ           PIC  X(003).
      *
               07  BA-SATZ-AM-LEBEN.
                 10  BA-PROVZEIT.
                     15  BA-PROVVON        PIC  9(004).
                     15  BA-PROVBIS        PIC  9(004).
                 10  BA-KUEDAT             PIC  9(006).
                 10  BA-STOGRUND           PIC  X(020).
                 10  BA-RUEDAT             PIC  9(006).
                 10  BA-LDYN.
                     15  BA-LDYNNR         PIC  9(002).
                     15  BA-LDYNDAT        PIC  9(006).
                 10  BA-VN.
                     15  BA-VNNAM          PIC  X(039).
                     15  BA-VNSTR          PIC  X(025).
180893*              15  BA-VNPLZ          PIC  X(009).
180200               15  BA-VNPLZ          PIC  X(006).
180200*              15  BA-VNORT          PIC  X(026).
                     15  BA-VNORT          PIC  X(029).
                     15  BA-VNGEBDAT       PIC  9(006).
                 10  BA-AG.
                     15  BA-AGNAM          PIC  X(039).
                     15  BA-AGSTR          PIC  X(025).
180893*              15  BA-AGPLZ          PIC  X(009).
180200               15  BA-AGPLZ          PIC  X(006).
180200*              15  BA-AGORT          PIC  X(026).
180200               15  BA-AGORT          PIC  X(029).
                 10  BA-ABT                PIC  X(001).
                 10  BA-LZE.
                     15  BA-LZEDAT         PIC  9(006).
                     15  BA-LZEBTG         PIC S9(007)V99   COMP-3.
                 10  BA-INKST              PIC  9(004).
                 10  BA-BTGRUECK           PIC S9(007)V99   COMP-3.
                 10  BA-BANKVERB.
                     15  BA-KTONR          PIC  9(010).
                     15  BA-BLZ            PIC  9(008).
                 10  BA-MAHNDAT            PIC  9(004).
                 10  BA-TARIF              PIC  X(012).
                 10  BA-VSU                PIC S9(009)V99   COMP-3.
                 10  BA-BEG                PIC  9(004).
                 10  BA-ABL                PIC  9(004).
                 10  BA-BTG                PIC S9(007)V99   COMP-3.
                 10  BA-ZAHLW              PIC  X(012).
                 10  BA-INKART             PIC  X(012).
                 10  BA-BFR                PIC S9(009)V99   COMP-3.
                 10  BA-RKW                PIC S9(009)V99   COMP-3.
                 10  BA-SE                 PIC S9(009)V99   COMP-3.
                 10  BA-GEWGUT             PIC S9(009)V99   COMP-3.
                 10  BA-BWSE               PIC S9(009)V99   COMP-3.
                 10  BA-VZMOEGL            PIC S9(009)V99   COMP-3.
                 10  BA-VZBETR             PIC S9(009)V99   COMP-3.
                 10  BA-VZZINS             PIC S9(007)V99   COMP-3.
                 10  BA-ZINSSATZ           PIC S9(003)V99   COMP-3.
                 10  BA-VERFUEG            PIC  X(012).
                 10  BA-RZBTG              PIC S9(007)V99   COMP-3.
                 10  BA-BETRKZ             PIC  9(001).
                 10  BA-VBNRALT            PIC  9(006).
                 10  BA-VBNRALT-NAM        PIC  X(060).
                 10  BA-GEWVA              PIC  X(001).
                 10  BA-ABLBZ              PIC  9(004).
211197           10  BA-BETREFF            PIC  X(001).
                 10  BA-RSGRP              PIC  X(006).
                 10  BA-VBTEL              PIC  X(014).
                 10  BA-VBFAX              PIC  X(014).
081196           10  BA-URSPRUNG           PIC  X(001).
120903           10  BA-BEITRAG-MIT-DYN    PIC S9(009)V99 COMP-3.
120903           10  BA-DYNABLKZN          PIC  9(002).
120903           10  BA-BTGVERF            PIC  X(001).
120903           10  BA-ERHPROZKZN         PIC  9(002).
120903           10  BA-BLZ-PRODGEBER      PIC  9(008).
120903           10  BA-KTONR-PRODGEBER    PIC  9(010).
180893*          10  FILLER                PIC  X(055).
120903           10  FILLER                PIC  X(026).
      *
               07  BA-SATZ-AM-VERS   REDEFINES  BA-SATZ-AM-LEBEN.
                 10  BA-V-ALLGEM.
                   12  BA-V-RUEDAT         PIC  9(006).
                   12  BA-VN.
                     15  BA-V-VNNAM        PIC  X(039).
                     15  BA-V-VNSTR        PIC  X(025).
                     15  BA-V-VNPLZ        PIC  X(006).
                     15  BA-V-VNORT        PIC  X(026).
                     15  BA-V-VNGEBDAT     PIC  9(006).
                   12  BA-V-VDIR.
                     15  BA-V-VDNR         PIC  X(003).
                     15  BA-V-VD-ORT       PIC  X(009).
                     15  BA-V-VD-TELNR     PIC  X(018).
                   12  BA-V-BTGRUECKD.
                     15  BA-V-BTGRUECKDM   PIC S9(007)V99   COMP-3.
                     15  BA-V-MAHNDAT      PIC  9(006).
                     15  BA-V-BTG-R-M      PIC S9(007)V99   COMP-3.
                   12  BA-V-BANKVERB.
                     15  BA-V-KTONR        PIC  X(010).
                     15  BA-V-BLZ          PIC  9(008).
                   12  BA-V-ZUST           PIC  X(010).
                   12  BA-V-BEG            PIC  9(006).
                   12  BA-V-ABL            PIC  9(006).
                   12  BA-V-ZAHLW          PIC  X(012).
                   12  BA-V-INKART         PIC  X(012).
                   12  BA-V-OSCHUTZ        PIC  9(006).
280294           10  BA-VERTRAGSDATEN      PIC  X(229).
280294*
280294**************** K F Z - D A T E N  ***************************
280294           10  BA-K-DATEN   REDEFINES  BA-VERTRAGSDATEN.
                   12  BA-V-KFZ-DATEN.
                     15  BA-V-AMTL-KZ      PIC  X(012).
                     15  BA-V-KFZART       PIC  9(003).
                     15  BA-V-VERWEND      PIC  X(025).
                     15  BA-V-HERST        PIC  X(016).
                     15  BA-V-LEIST-KW     PIC  9(005)      COMP-3.
                     15  BA-V-LEIST-PS     PIC  9(005)      COMP-3.
                   12  BA-V-BERUF          PIC  X(012).
                   12  BA-V-HPF-DATEN.
                     15  BA-V-DECK-H       PIC  X(026).
                     15  BA-V-JBTG-H       PIC S9(007)V99   COMP-3.
                     15  BA-V-SF-H         PIC  9(002).
                     15  BA-V-RAB-H        PIC  9(003)      COMP-3.
                   12  BA-V-VK-TK.
                     15  BA-V-VK           PIC  X(002).
                     15  BA-V-SB-VK        PIC  X(012).
                     15  BA-V-TK           PIC  X(002).
                     15  BA-V-SB-TK        PIC  X(012).
                     15  BA-V-JBTG-VK-TK   PIC S9(007)V99   COMP-3.
                     15  BA-V-SF-VK-TK     PIC  9(002).
                     15  BA-V-RAB-VK-TK    PIC  9(003)      COMP-3.
                     15  BA-V-SONDER       PIC  X(004).
                   12  BA-V-UNF.
                     15  BA-V-U-TOD        PIC  9(005).
                     15  BA-V-U-TOD-TXT    PIC  X(012).
                     15  BA-V-U-INV        PIC  9(005).
                     15  BA-V-U-TOD1       PIC  9(005).
                     15  BA-V-U-TOD1-TXT   PIC  X(012).
                     15  BA-V-U-INV1       PIC  9(005).
                     15  BA-V-JBTG-UNF     PIC S9(007)V99   COMP-3.
280294               15  BA-V-FILLER       PIC  X(030).
280294*
280294**************** U N F A L L - D A T E N  **********************
280294           10  BA-U-DATEN   REDEFINES  BA-VERTRAGSDATEN.
280294             12  BA-U-FILL           PIC  X(002).
280294             12  BA-U-VERS-PERS      PIC  X(021).
280294             12  BA-U-GEBDAT         PIC  9(006).
280294             12  BA-U-PRAEM          PIC  9(005)V99 COMP-3.
280294             12  BA-U-PRAEM-ZW       PIC  9(005)V99 COMP-3.
280294             12  BA-U-PROGR          PIC  9(003).
280294             12  BA-U-GRUNDS         PIC S9(007)    COMP-3.
280294             12  BA-U-VOLLINV        PIC S9(011)    COMP-3.
280294             12  BA-U-UEBERGANG      PIC S9(007)    COMP-3.
280294             12  BA-U-KTG            PIC S9(003)    COMP-3.
280294             12  BA-U-GG             PIC S9(003)    COMP-3.
280294             12  BA-U-KOSMETIK       PIC S9(005)    COMP-3.
280294             12  BA-U-TOD            PIC S9(009)    COMP-3.
280294             12  BA-U-BERGUNG        PIC S9(005)    COMP-3.
280294             12  BA-U-TG             PIC S9(003)    COMP-3.
280294             12  BA-U-ANGLPROZ       PIC  9(002)V99.
280294             12  BA-U-ANGLJAHR       PIC  X(004).
280294             12  BA-U-SONSTIGES      PIC  X(114).
280294             12  BA-U-WEIT-PERS      PIC  9(001).
280294             12  BA-U-FILLER         PIC  X(035).
      *
280294*
280294**************** H A U S R A T - D A T E N  ********************
280294           10  BA-HR-DATEN REDEFINES BA-VERTRAGSDATEN.
280294             12  BA-HR-FILL          PIC  X(002).
280294             12  BA-HR-LKZVORT       PIC  X(003).
280294             12  BA-HR-PLZVORT       PIC  X(005).
280294             12  BA-HR-VORT          PIC  X(021).
280294             12  BA-HR-VSTR          PIC  X(021).
280294             12  BA-HR-ZONE          PIC  9(001).
280294             12  BA-HR-BUCHREG       PIC  X(005).
280294             12  BA-HR-GEFAHR        PIC  X(012).
280294             12  BA-HR-HAUS          PIC  X(009).
280294             12  BA-HR-BEWOHNT       PIC  X(011).
280294             12  BA-HR-QM-WOHNFL     PIC S9(005)    COMP-3.
280294             12  BA-HR-VSUM          PIC S9(007)    COMP.
280294             12  BA-HR-PRAEM         PIC S9(005)V99 COMP-3.
280294             12  BA-HR-PRAEM-ZW      PIC S9(005)V99 COMP-3.
280294             12  BA-HR-MITVERS-PROZ.
280294               15  BA-HR-MIT-ART1    PIC S9(003)    COMP-3.
280294               15  BA-HR-MIT-PROZ1   PIC S9(002)V9  COMP-3.
280294               15  BA-HR-MIT-ART2    PIC S9(003)    COMP-3.
280294               15  BA-HR-MIT-PROZ2   PIC S9(002)V9  COMP-3.
280294               15  BA-HR-MIT-ART3    PIC S9(003)    COMP-3.
280294               15  BA-HR-MIT-PROZ3   PIC S9(002)V9  COMP-3.
280294               15  BA-HR-MIT-ART4    PIC S9(003)    COMP-3.
280294               15  BA-HR-MIT-PROZ4   PIC S9(002)V9  COMP-3.
280294               15  BA-HR-MIT-ART5    PIC S9(003)    COMP-3.
280294               15  BA-HR-MIT-PROZ5   PIC S9(002)V9  COMP-3.
280294             12  BA-HR-WEITER-KLAUS.
280294               15  BA-HR-KLAUSEL1    PIC  9(003).
280294               15  BA-HR-KLAUSEL2    PIC  9(003).
280294               15  BA-HR-KLAUSEL3    PIC  9(003).
280294               15  BA-HR-KLAUSEL4    PIC  9(003).
280294               15  BA-HR-KLAUSEL5    PIC  9(003).
280294             12  BA-HR-GLAS-PRAEM    PIC S9(005)V99 COMP-3.
280294             12  BA-HR-QM-GLAS       PIC S9(005)    COMP-3.
280294             12  BA-HR-BEZUG-GLAS    PIC  X(010).
280294             12  BA-HR-GLAS-GEB      PIC  X(009).
280294             12  BA-HR-ZUSATZWAGNIS.
280294               15  BA-HR-ZUSATZ1     PIC S9(003)    COMP-3.
280294               15  BA-HR-ZUSATZ2     PIC S9(003)    COMP-3.
280294               15  BA-HR-ZUSATZ3     PIC S9(003)    COMP-3.
280294               15  BA-HR-ZUSATZ4     PIC S9(003)    COMP-3.
280294               15  BA-HR-ZUSATZ5     PIC S9(003)    COMP-3.
280294             12  BA-HR-SONSTIGES     PIC  X(053).
      *
280294************** H A F T P L I C H T - D A T E N  ****************  
280294           10  BA-HP-DATEN  REDEFINES  BA-VERTRAGSDATEN.
280294             12  BA-HP-FILL          PIC  X(002).
280294             12  BA-HP-MITVERS-PERS  PIC  X(021).
280294             12  BA-HP-ART-HAFT      PIC  X(017).
280294             12  BA-HP-PRAEM         PIC S9(005)V99  COMP-3.
280294             12  BA-HP-PRAEM-ZW      PIC S9(005)V99  COMP-3.
280294             12  BA-HP-DECKSUM       PIC S9(001)     COMP-3.
280294             12  BA-HP-MIET          PIC S9(007)     COMP-3.
280294             12  BA-HP-VERMOEGEN     PIC S9(007)     COMP-3.
280294             12  BA-HP-SACH          PIC S9(007)     COMP-3.
280294             12  BA-HP-PERS          PIC S9(007)     COMP-3.
280294             12  BA-HP-E-PERS        PIC S9(007)     COMP-3.
280294             12  BA-HP-BAUHERR       PIC S9(005)     COMP-3.
280294             12  BA-HP-WEIT-RISIKO.
280294               15  BA-HP-WEIT-RIS1   PIC  X(020).
280294               15  BA-HP-WEIT-PRAEM1 PIC S9(005)V99  COMP-3.
280294               15  BA-HP-WEIT-RIS2   PIC  X(020).
280294               15  BA-HP-WEIT-PRAEM2 PIC S9(005)V99  COMP-3.
280294               15  BA-HP-WEIT-RIS3   PIC  X(020).
280294               15  BA-HP-WEIT-PRAEM3 PIC S9(005)V99  COMP-3.
280294               15  BA-HP-WEIT-RIS4   PIC  X(020).
280294               15  BA-HP-WEIT-PRAEM4 PIC S9(005)V99  COMP-3.
280294             12  BA-HP-SONSTIGES     PIC  X(061).
      *
      *
280294***********  W O H N G E B Ä U D E - D A T E N  ****************  
280294           10  BA-W-DATEN   REDEFINES  BA-VERTRAGSDATEN.
280294             12  BA-W-FILL           PIC  X(002).
280294             12  BA-W-LKZVORT        PIC  X(003).
280294             12  BA-W-PLZVORT        PIC  X(005).
280294             12  BA-W-VORT           PIC  X(021).
280294             12  BA-W-VSTR           PIC  X(021).
280294             12  BA-W-GEFAHR         PIC  X(013).
280294             12  BA-W-BEDING         PIC  X(018).
280294             12  BA-W-ZONE           PIC  9(001).
280294             12  BA-W-BKL            PIC  X(012).
280294             12  BA-W-GEBAEUDE       PIC  X(016).
280294             12  BA-W-BAUJAHR        PIC  X(020).
280294             12  BA-W-PRAEM          PIC S9(005)V99  COMP-3.
280294             12  BA-W-PRAEM-ZW       PIC S9(005)V99  COMP-3.
280294             12  BA-W-SUMMEN.
280294               15 BA-W-VSUM1         PIC S9(007)     COMP.
280294               15 BA-W-VSUM-NEB1     PIC S9(007)     COMP.
280294               15 BA-W-VSUM2         PIC S9(007)     COMP.
280294               15 BA-W-VSUM-NEB2     PIC S9(007)     COMP.
280294               15 BA-W-VSUM3         PIC S9(007)     COMP.
280294               15 BA-W-VSUM-NEB3     PIC S9(007)     COMP.
280294             12  BA-W-SONSTIGES      PIC  X(065).
      *
                 10  BA-V-SONST.
                     15  BA-V-BETRKZ       PIC  9(001).
                     15  BA-V-VBNRALT      PIC  9(006).
                     15  BA-V-VBNRALT-NAM  PIC  X(040).
                     15  BA-V-VBTEL        PIC  X(014).
                     15  BA-V-VBFAX        PIC  X(014).
                     15  FILLER            PIC  X(052).
      *
           05  BA-DUMMYSATZ     REDEFINES     BA-SATZ.
               07  BA-BEREINDAT        PIC 9(7)           COMP-3.
               07  BA-ANZNEU           PIC 9(7)           COMP-3.
               07  FILLER              PIC X(662).

