      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPC0005.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPC0005.CPv  $
      *
      *   Rev 2.3   Sep 29 2003 11:10:10   t13ex27
      *AKI00366 - Neue Besuchsauftrags-Art
      *
      *   Rev 2.2   12 Oct 2001 08:51:38   t13ex48
      *FKI00508
      *Fehler bei Waehrungs-KZ AML
      *
      *   Rev 2.1   15 Nov 2000 14:48:16   T13EX48
      *AKI00124
      *Nachtanwahl
      *
      *   Rev 2.0   Feb 27 1999 14:47:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   24 Sep 1998 16:54:36   T13AE19
      *FPC00035 - Anpassen BA-BETREFF von PIC 9   in  PIC X
      *
      *   Rev 1.2   24 Nov 1997 15:38:42   T13AE19
      *APC00032 - BA-BETREFF jetzt alphanummerisch
      *
      *   Rev 1.1   Jan 20 1997 11:23:10   T13AE19
      *BA-ART = 0 und BA-BETREFF PIC X
      *
      *   Rev 1.0   Nov 04 1995 09:02:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *****  VERSION   1    29.04.94 14:38:55
      *                                                                *
      *    NAME:  DRPC0005                  ERSTELLT VON: BORK         *
      *                                              AM : 01.03.94     *
      *                                                                *
      *    DIESES COPYMEMBER WIRD BENUTZT, UN DIE BA-DATEN AUS DER     *
      *    VSAM-DATEI IN DEN INFOPOOL EINZUSTELLEN.                    *
      *    ES KÖNNEN KEINE GEPACKTEN ZAHLEN EINGESTELLT WERDEN!!!!!!!  *
      *                                                                *
      *    AUS DRPC0001 EINGEFUEGT UND COMP-3 ENTFERNT.                *
      *    AENDERUNGEN IN DRPC0001 ANPASSEN.                           *
      *                                                                *
      *    DIE FELDER, DIE WIR VON AACHEN ALS TABELLE GELIEFERT        *
      *    BEKOMMEN, MÜSSEN AUFGELÖST WERDEN, DA SONST IN PMPC0012 DER *
      *    "MOVE CORR" NICHT FUNKTIONIERT.                             *
      *                                                                *
      *    LAENGE     = 762 BYTE                                       *
      *    DATEINAME  = UMXXXXXG DAT                                   *
      *----------------------------------------------------------------*
      *AUFTRAG !  ÄNDERUNGEN                          !NAME      !VER- *
      *DATUM   !                                      !          !SION *
      *--------!--------------------------------------!----------!-----*
      *AFKD0026!NEUERSTELLUNG, DIESE STRUKTUR WAR VOR-! BORK     ! 01  *
      *01.03.94!HER IM PROGRAMM DEFINIERT             !          !     *
      *--------!--------------------------------------!----------!-----*
      *AKI00124!Strukturaenderung der Felder:         ! Sebbach  ! 02  *
      *19.06.00! - BA-VNPLZ (Von PIC X(009) auf X(006)!          !     *
      *        ! - BA-AGPLZ (Von PIC X(009) auf X(006)!          !     *
      *        ! - BA-VNORT (Von PIC X(026) auf X(029)!          !     *
      *        ! - BA-AGORT (Von PIC X(026) auf X(029)!          !     *
      *----------------------------------------------------------------*
210901*21.09.01! Strukturaenderung des Feldes BA-PIF  ! Sebbach  !     *
      *FKI00508! hier sind verschiedene, nicht doku-  !          !     *
      *        ! mentierte Felder enthalten           !          !     *
      *----------------------------------------------------------------*
      *12.09.03! Neue Felder für BA-ART abgelehnte    ! Frenzel  !     *
      *AKI00366! Dynamik-Erhöhung                     !          !     *
      *--------!--------------------------------------!----------!-----*
      ***  DRPC0005.
       01  ZW-BEREICH-BA.
           05  BA-SCHL.
               10  BA-PCNR                PIC 9(005).
               10  BA-GES                 PIC 9(002).
               10  BA-VSNR                PIC 9(015).
               10  BA-ABSDAT              PIC 9(006).
               10  BA-GES-KZ              PIC 9(002).
           05  BA-SATZ.
               10  BA-STATKZ              PIC 9(001).
               10  BA-STATDAT             PIC 9(006).
               10  BA-ART                 PIC 9(001).
               10  BA-VBNR                PIC 9(007).
               10  BA-VBNAM               PIC X(060).
               10  BA-PIF                 PIC X(015).
210901         10  FILLER REDEFINES BA-PIF.
210901           15  BA-DIR                PIC  X(003).
210901           15  BA-FAELLIGKEIT        PIC  X(004).
210901           15  FILLER                PIC  X(005).
210901           15  BA-WAEHR-KZ           PIC  X(003).

              07  BA-SATZ-AM-LEBEN.
                  10 BA-PROVZEIT.
                     15 BA-PROVVON        PIC 9(004).
                     15 BA-PROVBIS        PIC 9(004).
                  10 BA-KUEDAT            PIC 9(006).
                  10 BA-STOGRUND          PIC X(020).
                  10 BA-RUEDAT            PIC 9(006).
                  10 BA-LDYN.
                     15 BA-LDYNNR         PIC 9(002).
                     15 BA-LDYNDAT        PIC 9(006).
                  10 BA-VN.
                     15 BA-VNNAM          PIC X(039).
                     15 BA-VNSTR          PIC X(025).
190600*              15 BA-VNPLZ          PIC X(009).
190600               15 BA-VNPLZ          PIC X(006).
190600*              15 BA-VNORT          PIC X(026).
190600               15 BA-VNORT          PIC X(029).
                     15 BA-VNGEBDAT       PIC 9(006).
                  10 BA-AG.
                     15 BA-AGNAM          PIC X(039).
                     15 BA-AGSTR          PIC X(025).
190600*              15 BA-AGPLZ          PIC X(009).
190600               15 BA-AGPLZ          PIC X(006).
190600*              15 BA-AGORT          PIC X(026).
190600               15 BA-AGORT          PIC X(029).
                  10 BA-ABT               PIC X(001).
                  10 BA-LZE.
                     15  BA-LZEDAT        PIC 9(006).
                     15  BA-LZEBTG        PIC 9(007)V99.
                  10 BA-INKST             PIC 9(004).
                  10 BA-BTGRUECK          PIC 9(007)V99.
                  10 BA-BANKVERB.
                     15  BA-KTONR         PIC 9(010).
                     15  BA-BLZ           PIC 9(008).
                  10 BA-MAHNDAT           PIC 9(004).
                  10 BA-TARIF             PIC X(012).
                  10 BA-VSU               PIC 9(009)V99.
                  10 BA-BEG               PIC 9(004).
                  10 BA-ABL               PIC 9(004).
                  10 BA-BTG               PIC 9(007)V99.
                  10 BA-ZAHLW             PIC X(012).
                  10 BA-INKART            PIC X(012).
                  10 BA-BFR               PIC 9(009)V99.
                  10 BA-RKW               PIC 9(009)V99.
                  10 BA-SE                PIC 9(009)V99.
                  10 BA-GEWGUT            PIC 9(009)V99.
                  10 BA-BWSE              PIC 9(009)V99.
                  10 BA-VZMOEGL           PIC 9(009)V99.
                  10 BA-VZBETR            PIC 9(009)V99.
                  10 BA-VZZINS            PIC 9(007)V99.
                  10 BA-ZINSSATZ          PIC 9(003)V99.
                  10 BA-VERFUEG           PIC X(012).
                  10 BA-RZBTG             PIC 9(007)V99.
                  10 BA-BETRKZ            PIC 9(001).
                  10 BA-VBNRALT           PIC 9(006).
                  10 BA-VBNRALT-NAM       PIC X(060).
130893            10 BA-GEWVA             PIC X(001).
130893            10 BA-ABLBZ             PIC 9(004).
240998            10 BA-BETREFF           PIC X(001).
130893            10 BA-RSGRP             PIC X(006).
130893            10 BA-VBTEL             PIC X(014).
130893            10 BA-VBFAX             PIC X(014).
281196            10 BA-URSPRUNG          PIC X(001).
120903           10  BA-BEITRAG-MIT-DYN    PIC S9(009)V99.
120903           10  BA-DYNABLKZN          PIC  9(002).
120903           10  BA-BTGVERF            PIC  X(001).
120903           10  BA-ERHPROZKZN         PIC  9(002).
120903           10  BA-BLZ-PRODGEBER      PIC  9(008).
120903           10  BA-KTONR-PRODGEBER    PIC  9(010).
281196*          10  FILLER                PIC  X(055).
120903           10  FILLER                PIC  X(021).
      *
             07  BA-SATZ-AM-VERS   REDEFINES  BA-SATZ-AM-LEBEN.
010394          10 BA-V-ALLGEM.
                  12 BA-V-RUEDAT           PIC  9(006).
                  12 BA-VN.
                    15 BA-V-VNNAM          PIC  X(039).
                    15 BA-V-VNSTR          PIC  X(025).
                    15 BA-V-VNPLZ          PIC  X(006).
                    15 BA-V-VNORT          PIC  X(026).
                    15 BA-V-VNGEBDAT       PIC  9(006).
                  12 BA-V-VDIR.
                    15 BA-V-VDNR           PIC  X(003).
                    15 BA-V-VD-ORT         PIC  X(009).
                    15 BA-V-VD-TELNR       PIC  X(018).
                  12 BA-V-BTGRUECKD.
                    15 BA-V-BTGRUECKDM     PIC  9(007)V99.
                    15 BA-V-MAHNDAT        PIC  9(006).
                    15 BA-V-BTG-R-M        PIC  9(007)V99.
                  12 BA-V-BANKVERB.
                    15 BA-V-KTONR          PIC  X(010).
                    15 BA-V-BLZ            PIC  9(008).
                  12 BA-V-ZUST             PIC  X(010).
                  12 BA-V-BEG              PIC  9(006).
                  12 BA-V-ABL              PIC  9(006).
                  12 BA-V-ZAHLW            PIC  X(012).
                  12 BA-V-INKART           PIC  X(012).
                  12 BA-V-OSCHUTZ          PIC  9(006).
010394          10 BA-VERTRAGSDATEN        PIC  X(270).

010394**************   K F Z - D A T E N   *** 217 BYTE *************
010394          10 BA-K-DATEN  REDEFINES   BA-VERTRAGSDATEN.
                 12 BA-V-KFZ-DATEN.
                   15 BA-V-AMTL-KZ         PIC  X(012).
                   15 BA-V-KFZART          PIC  9(003).
                   15 BA-V-VERWEND         PIC  X(025).
                   15 BA-V-HERST           PIC  X(016).
                   15 BA-V-LEIST-KW        PIC  9(005).
                   15 BA-V-LEIST-PS        PIC  9(005).
                 12 BA-V-BERUF             PIC  X(012).
                 12 BA-V-HPF-DATEN.
                   15 BA-V-DECK-H          PIC  X(026).
                   15 BA-V-JBTG-H          PIC  9(007)V99.
                   15 BA-V-SF-H            PIC  9(002).
                   15 BA-V-RAB-H           PIC  9(003).
                 12 BA-V-VK-TK.
                   15 BA-V-VK              PIC  X(002).
                   15 BA-V-SB-VK           PIC  X(012).
                   15 BA-V-TK              PIC  X(002).
                   15 BA-V-SB-TK           PIC  X(012).
                   15 BA-V-JBTG-VK-TK      PIC  9(007)V99.
                   15 BA-V-SF-VK-TK        PIC  9(002).
                   15 BA-V-RAB-VK-TK       PIC  9(003).
                   15 BA-V-SONDER          PIC  X(004).
                 12 BA-V-UNF.
                   15 BA-V-U-TOD           PIC  9(005).
                   15 BA-V-U-TOD-TXT       PIC  X(012).
                   15 BA-V-U-INV           PIC  9(005).
                   15 BA-V-U-TOD1          PIC  9(005).
                   15 BA-V-U-TOD1-TXT      PIC  X(012).
                   15 BA-V-U-INV1          PIC  9(005).
                   15 BA-V-JBTG-UNF        PIC  9(007)V99.
                   15 BA-V-FILLER          PIC  X(053).
010394*
010394**************** U N F A L L - D A T E N  *** 222 BYTE *********
010394           10  BA-U-DATEN   REDEFINES  BA-VERTRAGSDATEN.
010394             12  BA-U-FILL           PIC  X(002).
010394             12  BA-U-VERS-PERS      PIC  X(021).
010394             12  BA-U-GEBDAT         PIC  9(006).
010394             12  BA-U-PRAEM          PIC  9(005)V99.
010394             12  BA-U-PRAEM-ZW       PIC  9(005)V99.
010394             12  BA-U-PROGR          PIC  9(003).
010394             12  BA-U-GRUNDS         PIC  9(007).
010394             12  BA-U-VOLLINV        PIC  9(011).
010394             12  BA-U-UEBERGANG      PIC  9(007).
010394             12  BA-U-KTG            PIC  9(003).
010394             12  BA-U-GG             PIC  9(003).
010394             12  BA-U-KOSMETIK       PIC  9(005).
010394             12  BA-U-TOD            PIC  9(009).
010394             12  BA-U-BERGUNG        PIC  9(005).
010394             12  BA-U-TG             PIC  9(003).
010394             12  BA-U-ANGLPROZ       PIC  9(002)V99.
010394             12  BA-U-ANGLJAHR       PIC  X(004).
010394             12  BA-U-SONSTIGES      PIC  X(114).
010394             12  BA-U-WEIT-PERS      PIC  9(001).
010394             12  BA-U-FILLER         PIC  X(048).
010394*
010394**************** H A U S R A T - D A T E N  ****** 263 BYTE ****
010394           10  BA-HR-DATEN REDEFINES BA-VERTRAGSDATEN.
010394             12  BA-HR-FILL          PIC  X(002).
010394             12  BA-HR-LKZVORT       PIC  X(003).
010394             12  BA-HR-PLZVORT       PIC  X(005).
010394             12  BA-HR-VORT          PIC  X(021).
010394             12  BA-HR-VSTR          PIC  X(021).
010394             12  BA-HR-ZONE          PIC  9(001).
010394             12  BA-HR-BUCHREG       PIC  X(005).
010394             12  BA-HR-GEFAHR        PIC  X(012).
010394             12  BA-HR-HAUS          PIC  X(009).
010394             12  BA-HR-BEWOHNT       PIC  X(011).
010394             12  BA-HR-QM-WOHNFL     PIC  9(005).
010394             12  BA-HR-VSUM          PIC  9(010).
010394             12  BA-HR-PRAEM         PIC  9(005)V99.
010394             12  BA-HR-PRAEM-ZW      PIC  9(005)V99.
010394             12  BA-HR-MITVERS-PROZ.
010394               15  BA-HR-MIT-ART1    PIC  9(003).
010394               15  BA-HR-MIT-PROZ1   PIC  9(002)V9.
010394               15  BA-HR-MIT-ART2    PIC  9(003).
010394               15  BA-HR-MIT-PROZ2   PIC  9(002)V9.
010394               15  BA-HR-MIT-ART3    PIC  9(003).
010394               15  BA-HR-MIT-PROZ3   PIC  9(002)V9.
010394               15  BA-HR-MIT-ART4    PIC  9(003).
010394               15  BA-HR-MIT-PROZ4   PIC  9(002)V9.
010394               15  BA-HR-MIT-ART5    PIC  9(003).
010394               15  BA-HR-MIT-PROZ5   PIC  9(002)V9.
010394             12  BA-HR-WEITER-KLAUS.
010394               15  BA-HR-KLAUSEL1    PIC  9(003).
010394               15  BA-HR-KLAUSEL2    PIC  9(003).
010394               15  BA-HR-KLAUSEL3    PIC  9(003).
010394               15  BA-HR-KLAUSEL4    PIC  9(003).
010394               15  BA-HR-KLAUSEL5    PIC  9(003).
010394             12  BA-HR-GLAS-PRAEM    PIC  9(005)V99.
010394             12  BA-HR-QM-GLAS       PIC  9(005).
010394             12  BA-HR-BEZUG-GLAS    PIC  X(010).
010394             12  BA-HR-GLAS-GEB      PIC  X(009).
010394             12  BA-HR-ZUSATZWAGNIS.
010394               15  BA-HR-ZUSATZ1     PIC  9(003).
010394               15  BA-HR-ZUSATZ2     PIC  9(003).
010394               15  BA-HR-ZUSATZ3     PIC  9(003).
010394               15  BA-HR-ZUSATZ4     PIC  9(003).
010394               15  BA-HR-ZUSATZ5     PIC  9(003).
010394             12  BA-HR-SONSTIGES     PIC  X(053).
010394             12  BA-HR-FILLER        PIC  X(007).
      *
010394************** H A F T P L I C H T - D A T E N  *** 264 BYTE ***  
010394           10  BA-HP-DATEN  REDEFINES  BA-VERTRAGSDATEN.
010394             12  BA-HP-FILL          PIC  X(002).
010394             12  BA-HP-MITVERS-PERS  PIC  X(021).
010394             12  BA-HP-ART-HAFT      PIC  X(017).
010394             12  BA-HP-PRAEM         PIC  9(005)V99.
010394             12  BA-HP-PRAEM-ZW      PIC  9(005)V99.
010394             12  BA-HP-DECKSUM       PIC  9(001).
010394             12  BA-HP-MIET          PIC  9(007).
010394             12  BA-HP-VERMOEGEN     PIC  9(007).
010394             12  BA-HP-SACH          PIC  9(007).
010394             12  BA-HP-PERS          PIC  9(007).
010394             12  BA-HP-E-PERS        PIC  9(007).
010394             12  BA-HP-BAUHERR       PIC  9(005).
010394             12  BA-HP-WEIT-RISIKO.
010394               15  BA-HP-WEIT-RIS1   PIC  X(020).
010394               15  BA-HP-WEIT-PRAEM1 PIC  9(005)V99.
010394               15  BA-HP-WEIT-RIS2   PIC  X(020).
010394               15  BA-HP-WEIT-PRAEM2 PIC  9(005)V99.
010394               15  BA-HP-WEIT-RIS3   PIC  X(020).
010394               15  BA-HP-WEIT-PRAEM3 PIC  9(005)V99.
010394               15  BA-HP-WEIT-RIS4   PIC  X(020).
010394               15  BA-HP-WEIT-PRAEM4 PIC  9(005)V99.
010394             12  BA-HP-SONSTIGES     PIC  X(061).
010394             12  BA-HP-FILLER        PIC  X(006).
      *
010394***********  W O H N G E B Ä U D E - D A T E N  *** 268 BYTE ***  
010394           10  BA-W-DATEN   REDEFINES  BA-VERTRAGSDATEN.
010394             12  BA-W-FILL           PIC  X(002).
010394             12  BA-W-LKZVORT        PIC  X(003).
010394             12  BA-W-PLZVORT        PIC  X(005).
010394             12  BA-W-VORT           PIC  X(021).
010394             12  BA-W-VSTR           PIC  X(021).
010394             12  BA-W-GEFAHR         PIC  X(013).
010394             12  BA-W-BEDING         PIC  X(018).
010394             12  BA-W-ZONE           PIC  9(001).
010394             12  BA-W-BKL            PIC  X(012).
010394             12  BA-W-GEBAEUDE       PIC  X(016).
010394             12  BA-W-BAUJAHR        PIC  X(020).
010394             12  BA-W-PRAEM          PIC  9(005)V99.
010394             12  BA-W-PRAEM-ZW       PIC  9(005)V99.
010394             12  BA-W-SUMMEN.
010394               15 BA-W-VSUM1         PIC  9(010).
010394               15 BA-W-VSUM-NEB1     PIC  9(009).
010394               15 BA-W-VSUM2         PIC  9(010).
010394               15 BA-W-VSUM-NEB2     PIC  9(009).
010394               15 BA-W-VSUM3         PIC  9(010).
010394               15 BA-W-VSUM-NEB3     PIC  9(009).
010394             12  BA-W-SONSTIGES      PIC  X(065).
010394             12  BA-W-FILLER         PIC  X(002).
010394
010394*********  ENDE DER REDEFINIERTEN SPARTENDATEN  ****************
010394
                10 BA-V-SONST.
                   15 BA-V-BETRKZ          PIC  9(001).
                   15 BA-V-VBNRALT         PIC  9(006).
010394             15 BA-V-VBNRALT-NAM     PIC  X(060).
                   15 BA-V-VBTEL           PIC  X(014).
                   15 BA-V-VBFAX           PIC  X(014).
010394             15 BA-V-FILLER          PIC  X(045).
