      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 2 6 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  AUSGABE AUS AUFBEREITUNGSPROGRAMMEN AM-VERS,                  *
      *  COPYSTRECKE FUER VERTRAGSDATEN kfz.                           *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *    SATZLÄNGE   1800                                            *
150908*    SATZLÄNGE   2150                                            *
300910*    SATZLÄNGE   2600                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.06.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *29.07.98!PICTURE-Definition von "KZ"-Feldern    !KH Müller! 02  *
      *        !korrigiert sowie von SF-KLASSE-KASKO   !         !     *
      *        !                 und SF-KLASSE-KH      !         !     *
      *        !                                       !         !     *
      *02.12.98!Falsche Feld-Def. für ID-Felder korr.  !WGR      !     *
      *        !                                       !         !     *
      *05.10.04!Neues Feld Datum Erstzulassung VN      !W.Morfeld! 03  *
      *        !Erhöhung des Filler Neue SL 300        !         !     *
      *        !                                       !         !     *
AKI553*AKI00553 Neue Felder f. Bestandsdatenerwei-     !W.Misgeld!     *
      *        !terung Kfz-Daten                       !         !     *
      *        !                                       !         !     *
150908*AKI00731! Bestandsdatenerw. Kfz-Tarifierung     !W.Misgeld!     *
      *        !                                       !         !     *
300910*AKI00995! Dynamische Anzeige 3 neue Felder      !Hübner   !     *
      *        !                                       !         !     *
120711*AKI01041! Erweiterung d. Leistungskennzeichen   !WMI      !     *
      *        !                                       !         !     *
120711*AKI01131! neue Felder, Wegfall Felder f. KU     !BST      !     *
      *        !                                       !         !     *
300915*AKI01381! Neue Felder Moped Paket KTY/KNR       !HHU      !     *
      *        !                                       !         !     *
210416*AKI01427! Neue Felder für NGT 2016              !tfl      !     *
      *        !                                       !         !     *
030517*AKI01517! DTA überarbeiten                      !HHU      !
      *        !                                       !         !
      *AKI01779! neue Felder FIRMEN-NR E-FZG-KZ        ! HHU     !
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10064  !31.05.22! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    KFZ-GEBDAT-VN             !          
      *            !        !    KFZ-AEND-GEBDAT-VN-KZ     !          
      *            !        ! alte Felder gelöscht         !          
      *            !        !    KFZ-FALTERMAX             !          
      *            !        !    KFZ-AEND-FALTERMAX-KZ     !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ******************************************************************
 
       01  DRKI0267.
 
           05  KFZ-GES                     PIC  9(002).
           05  KFZ-VNR                     PIC  9(015).
 
           05  KFZ-ZWEIG                   PIC  9(002).
           05  KFZ-ZWEIG-TEXT              PIC  X(003).
 
           05  KFZ-PRODUKT-ID              PIC  9(002).
 
      *        Inkassoart LSV Scoring
           05  KFZ-LSV-SCOR                PIC  X(001).
           05  KFZ-AEND-LSV-SCOR-KZ        PIC  X(001).
      *        = 'J', wenn geändert
 
      *        Amtliches Kennzeichen
           05  KFZ-AMT-KZ                  PIC  X(012).
           05  KFZ-AEND-AMT-KZ-KZ          PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-AKZART-KNR              PIC  9(003).
           05  KFZ-AEND-AKZART-KNR-KZ      PIC  X(001).
           05  KFZ-SAISON-KZ               PIC  X(001).
           05  KFZ-AEND-SAISON-KZ-KZ       PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-SAISONBEGINN            PIC  9(002).
           05  KFZ-AEND-SAISONBEGINN-KZ    PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-SAISONENDE              PIC  9(002).
           05  KFZ-AEND-SAISONENDE-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-ZULASSDATUM             PIC  X(010).
           05  KFZ-AEND-ZULASSDATUM-KZ     PIC  X(001).
      *        = 'J', wenn geändert
051004*        Datum Erstzulassung VN
051004     05  KFZ-EZDATVN                 PIC  9(008).
051004     05  KFZ-AEND-EZDATVN-KZ         PIC  X(001).
051004*        = 'J', wenn geändert
           05  KFZ-FZGART-KNR              PIC  9(003).
           05  KFZ-AEND-FZGART-KNR-KZ      PIC  X(001).
           05  KFZ-VERWEND-KNR             PIC  9(003).
           05  KFZ-AEND-VERWEND-KNR-KZ     PIC  X(001).
           05  KFZ-NAEHSPEZ-KNR            PIC  9(003).
           05  KFZ-AEND-NAEHSPEZ-KNR-KZ    PIC  X(001).
 
      *        Fahrzeugtyp aus Wagniskennzeichen von AMV (KFZ-Wagniskz)
           05  KFZ-SCHL-FAHRZTYP           PIC  S9(4) BINARY.
           05  KFZ-AEND-SCHL-FAHRZTYP-KZ   PIC  X(001).
      *        = 'J', wenn geändert
      *        Herstellungsschlüssel
           05  KFZ-HERST-NR                PIC  X(004).
           05  KFZ-AEND-HERST-NR-KZ        PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-HERSTELLTEXT            PIC  X(025).
           05  KFZ-AEND-HERSTELLTEXT-KZ    PIC  X(001).
      *        Herstellermodell
           05  KFZ-TYP-MOD-TEXT            PIC   X(020).
           05  KFZ-AEND-TYP-MOD-TEXT-KZ    PIC   X(001).
      *        = 'J', wenn geändert
      *        KFZ-Schlüssel
           05  KFZ-KZ-KFZ-TYP              PIC  X(003).
           05  KFZ-AEND-KZ-KFZ-TYP-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-FAHRZ-ID                PIC  X(020).
           05  KFZ-AEND-FAHRZ-ID-KZ        PIC  X(001).
      *        = 'J', wenn geändert
      *        Leistungs-Schlüssel
           05  KFZ-LEISTUNG-ARTID          PIC  S9(4) BINARY.
           05  KFZ-AEND-LEISTUNG-ARTID-KZ  PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-LEISTUNG                PIC  9(005)V99.
           05  KFZ-AEND-LEISTUNG-KZ        PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-PLATZ                   PIC S9(003).
           05  KFZ-AEND-PLATZ-KZ           PIC  X(001).
      *        J = Änderung Anzahl Plätze
           05  KFZ-GESGEW                  PIC S9(007).
           05  KFZ-AEND-GESGEW-KZ          PIC  X(001).
      *        J = Änderung Gesamtgewicht
           05  KFZ-KW                      PIC S9(004).
           05  KFZ-AEND-KW-KZ              PIC  X(001).
      *        J = Änderung KW-Leistung
           05  KFZ-NUTZ                    PIC S9(007).
    <<     05  KFZ-AEND-NUTZ-KZ            PIC  X(001).
120711*        J = Änderung Nutzlast
      *        Gesamtneuwert des Fahrzeugs
           05  KFZ-GES-NEUWERT             PIC  9(007).
           05  KFZ-AEND-GES-NEUWERT-KZ     PIC  X(001).
      *        = 'J', wenn geändert
           05  FILLER                      PIC  X(008).
           05  KFZ-JAHR-FAHRLEIST          PIC  9(007).
           05  KFZ-AEND-JAHR-FAHRLEIST-KZ  PIC  X(001).
      *        = 'J', wenn geändert
300910     05  KFZ-KM-STAND-AKT               PIC S9(7).
           05  KFZ-AEND-KM-STAND-AKT-KZ       PIC  X(001).
      *        J = Änderung KM-Stand aktuell
           05  KFZ-KM-STAND-AKT-DATUM         PIC  X(10).
           05  KFZ-AEND-KM-STAND-AKT-DATUM-KZ PIC  X(001).
      *        J = Änderung KM-Stand aktuell Datum
           05  KFZ-FAHRER                  PIC  X(010).
           05  KFZ-AEND-FAHRER-KZ          PIC  X(001).
      *        = 'J', wenn geändert
      *        Geburtsdaten
           05  KFZ-GEBDAT-FAHRER           PIC  X(010).
           05  KFZ-AEND-GEBDAT-FAHRER-KZ   PIC  X(001).
      *        = 'J', wenn geändert
      *    05  KFZ-GEBDAT-FAHRERIN         PIC  X(010).
      *    05  KFZ-AEND-GEBDAT-FAHRERIN-KZ PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-ALTERSGRP-KNR           PIC S9(004).
           05  KFZ-AEND-ALTERSGRP-KNR-KZ   PIC  X(001).
      *        Begleitetes Fahren
           05  KFZ-BEGL-FAHRER             PIC S9(002).
           05  KFZ-AEND-BEGL-FAHRER-KZ     PIC  X(001).
      *        = 'J' wenn geändert
           05  KFZ-BEGL-FAHRER-KTX         PIC  X(010).
           05  KFZ-AEND-BEGL-FAHRER-KTX-KZ PIC  X(001).
      *        = 'J' wenn geändert
           05  KFZ-FZG-HALTER              PIC S9(002).
           05  KFZ-AEND-FZG-HALTER-KZ      PIC  X(001).
      *        = 'J' wenn geändert
           05  KFZ-FZG-HALTER-KTX          PIC  X(010).
           05  KFZ-AEND-FZG-HALTER-KTX-KZ  PIC  X(001).
      *        = 'J' wenn geändert
      *        abweichender Halter-Name
           05  KFZ-HALT-NNAME              PIC  X(040).
           05  KFZ-AEND-HALT-NNAME-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-HALT-VNAME              PIC  X(040).
           05  KFZ-AEND-HALT-VNAME-KZ      PIC  X(001).
      *        = 'J', wenn geändert
      *        sonstige Vertragsangaben
           05  KFZ-FZG-NUTZ                PIC  X(010).
           05  KFZ-AEND-FZG-NUTZ-KZ        PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-FZG-ABSTELL             PIC  X(010).
           05  KFZ-AEND-FZG-ABSTELL-KZ     PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-AUFBAUART-KNR           PIC S9(004).
           05  KFZ-AEND-AUFBAUART-KNR-KZ   PIC  X(001).
           05  KFZ-FAHRGEBIET              PIC  X(010).
           05  KFZ-AEND-FAHRGEBIET-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-FUEHRSCH-ERWERB         PIC  X(010) .
           05  KFZ-AEND-FUEHRSCH-ERWERB-KZ PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-WOHNEIGENT              PIC  X(010).
           05  KFZ-AEND-WOHNEIGENT-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-AG-BRANCHE              PIC  X(060).
           05  KFZ-AEND-AG-BRANCHE-KZ      PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-SONDERAUSSTATT-TAB.
               10  KFZ-SONDERAUSSTATT            OCCURS 20.
                   15 KFZ-SONTEILNR-KNR       PIC S9(003).
                   15 KFZ-AEND-SONTEIL-KNR-KZ PIC  X(001).
                   15 KFZ-SONTEILEWERT        PIC S9(007)V99.
                   15 KFZ-AEND-SONTEILEWER-KZ PIC  X(001).
150908**** Kfz-Tarifierung
           05  KFZ-TARIFAUSWAHL            PIC  X(001).
           05  KFZ-AEND-TARIFAUSWAHL-KZ    PIC  X(001).
>>         05  KFZ-WERKSTBINDKZ            PIC  X(001).
           05  KFZ-AEND-WERKSTBINDKZ-KZ    PIC  X(001).
      *        Deckungschlüssel Haftpflicht
           05  KFZ-DA-KH                   PIC  9(2).
           05  KFZ-AEND-DA-KH-KZ           PIC  X(001).
      *        = 'J', wenn geändert
      *        Regionalklasse Haftpflicht
           05  KFZ-REG-KLASSE-KH           PIC  X(003).
           05  KFZ-AEND-REG-KLASSE-KH-KZ   PIC  X(001).
      *        = 'J', wenn geändert
      *        Typklasse Haftpflicht
           05  KFZ-TYPKLASSE-HP            PIC  9(002).
           05  KFZ-AEND-TYPKLASSE-HP-KZ    PIC  X(001).
      *        = 'J', wenn geändert
      *        Haftpflichtbeitrag in DM (KFZ-BEITRAG-ZW-KH)
           05  KFZ-BEITRAG-KH              PIC S9(008)V99.
           05  KFZ-AEND-BEITRAG-KH-KZ      PIC  X(001).
      *        = 'J', wenn geändert
      *        Kaskoart aus Deckungschlüssel Fahrzeug (KFZ-DA-KF)
150908*    05  KFZ-KASKOART                PIC  X(001).
150908     05  KFZ-KASKOART                PIC  9(002).
           05  KFZ-AEND-KASKOART-KZ        PIC  X(001).
      *        = 'J', wenn geändert
      *        Regionalklasse Fahrzeug
           05  KFZ-REG-KLASSE-KF           PIC  X(003).
           05  KFZ-AEND-REG-KL-KF-KZ       PIC  X(001).
      *        = 'J', wenn geändert
      *        Typklasse Fahrzeug
           05  KFZ-TYPKLASSE-KF            PIC  9(002).
           05  KFZ-AEND-TYPKLASSE-KF-KZ    PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-SB-VK-TK                PIC  X(001).
           05  KFZ-AEND-SB-VK-TK-KZ        PIC  X(001).
      *        = 'J', wenn geändert
      *        Selbstbeteiligung Vollkasko
           05  KFZ-SB-VK                   PIC S9(008)V99.
           05  KFZ-AEND-SB-VK-KZ           PIC  X(001).
      *        = 'J', wenn geändert
      *        Selbstbeteiligung Teilkasko
           05  KFZ-SB-TK                   PIC S9(008)V99.
           05  KFZ-AEND-SB-TK-KZ           PIC  X(001).
      *        = 'J', wenn geändert
      *        Fahrzeugbeitrag in DM (KFZ-BEITRAG-ZW-KF)
           05  KFZ-BEITRAG-KF              PIC S9(008)V99.
           05  KFZ-AEND-BEITRAG-KF-KZ      PIC  X(001).
      *        = 'J', wenn geändert
    <<     05  KFZ-LEASING                 PIC S9(001).
150908     05  KFZ-AEND-LEASING-KZ         PIC  X(001).
           05  KFZ-SCHUTZBRIEF             PIC  X(010).
           05  KFZ-AEND-SCHUTZBRIEF-KZ     PIC  X(001).
      *        = 'J', wenn geändert
      *        Schadensfreiheitsrabatt
120711     05  KFZ-SFR-FREI-KH             PIC  X(001).
>>         05  KFZ-AEND-SFR-FREI-KH-KZ     PIC  X(001).
      *        J = Änderung SFR-FREI-Haftpflicht
           05  KFZ-SFANZKH                 PIC  9(003).
           05  KFZ-AEND-SFANZKH-KZ         PIC  X(001).
           05  KFZ-PSFANZKH                PIC S9(002).
           05  KFZ-AEND-PSFANZKH-KZ        PIC  X(001).
      *        Haftpflichtbeitrag in Prozent
           05  KFZ-BEITRAGKH-PROZ          PIC  9(003).
           05  KFZ-AEND-BEITRAGKH-PROZ-KZ  PIC  X(001).
      *        = 'J', wenn geändert
      *        Schadenfreiheitsklasse Haftpflicht (KFZ-BEITRAGKL-KH)
           05  KFZ-SF-KLASSE-KH            PIC  S9(4) BINARY.
           05  KFZ-AEND-SF-KLASSE-KH-KZ    PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-SFR-FREI-VK             PIC  X(001).
           05  KFZ-AEND-SFR-FREI-VK-KZ     PIC  X(001).
      *        J = Änderung SFR-FREI-Vollkasko
           05  KFZ-SFANZVK                 PIC  9(003).
           05  KFZ-AEND-SFANZVK-KZ         PIC  X(001).
           05  KFZ-PSFANZVK                PIC S9(002).
           05  KFZ-AEND-PSFANZVK-KZ        PIC  X(001).
      *        Fahrzeugbeitrag in Prozent
           05  KFZ-BEITRAGKF-PROZ          PIC  9(003).
           05  KFZ-AEND-BEITRAGKF-PROZ-KZ  PIC  X(001).
      *        = 'J', wenn geändert
      *        Schadenfreiheitsklasse Vollkaso/Teilkasko (KFZ-BKL-VK)
           05  KFZ-SF-KLASSE-KASKO         PIC  S9(4) BINARY.
           05  KFZ-AEND-SF-KL-KASKO-KZ     PIC  X(001).
      *        = 'J', wenn geändert
      *        Tarifbezeichnung
           05  KFZ-TARIF-BEZ               PIC  X(007).
           05  KFZ-AEND-TARIF-BEZ-KZ       PIC  X(001).
      *        = 'J', wenn geändert
           05  KFZ-KSMVERTRAG-TAB.
               10  KFZ-KSMVERTRAG             OCCURS 4.
                   15  KFZ-TAR-BST-SPART   PIC  X(006).
                   15  KFZ-TAR-BST-KTX     PIC  X(010).
                   15  KFZ-TAR-BST-TXT     PIC  X(050).
                   15  KFZ-TAR-BST-GDVKZ   PIC  X(050).
                   15  KFZ-AEND-KSMVERT-KZ PIC  X(001).
      *        = 'J', wenn geändert
      *        Kenntexte
           05  KFZ-BES-VEREINBARUNG-TAB.
               10  KFZ-BES-VEREINBARUNG       OCCURS 15.
                   15  KFZ-BESVEREINB       PIC  X(050).
                   15  KFZ-AEND-BESVEREI-KZ PIC  X(001).
      *        = 'J', wenn geändert
 
300915     05  KFZ-MOPED-PAKET-TYP-KNR        PIC  9(004).
300915     05  KFZ-AEND-MOPED-PAKET-TYP-KZ    PIC  X(001).
210416     05  KFZ-TELEMATIK-ID               PIC  9(014).
210416     05  KFZ-AEND-TELEMATIK-ID-KZ       PIC  X(001).
210416     05  KFZ-SCOREWERT                  PIC  9(003).
210416     05  KFZ-AEND-SCOREWERT-KZ          PIC  X(001).
210416     05  KFZ-SCORE-DATUM                PIC  X(010).
210416     05  KFZ-AEND-SCORE-DATUM-KZ        PIC  X(001).
210416     05  KFZ-FALTERMAX-KNR              PIC S9(002).
210416     05  KFZ-AEND-FALTERMAX-KNR-KZ      PIC  X(001).
           05  KFZ-FIRMEN-NR                  PIC  X(006).
           05  KFZ-AEND-FIRMEN-NR-KZ          PIC  X(001).
           05  KFZ-E-FZG-KZ                   PIC  X(001).
           05  KFZ-AEND-E-FZG-KZ-KZ           PIC  X(001).
           05  KFZ-GEBDAT-VN                  PIC  X(010).
           05  KFZ-AEND-GEBDAT-VN-KZ          PIC  X(001).
210416     05  FILLER                         PIC  X(405).
           05  KFZ-AKTUAL-KZ                  PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      ******************************************************************
      *          E n d e    Copy-Buch   DRKI0267                       *
      ******************************************************************