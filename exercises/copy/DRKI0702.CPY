      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Eingabedatei AMV-Vertragsdaten für KFZ mit Vorsatz      *
      *                                                               *
      *    SATZLÄNGE   3915 Byte                                      *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *21.10.98!NEUERSTELLUNG                         !B.GOLÜCKE! 01  *
      *        !                                      !         !     *
250403*AKI00338!Änderungen von Feldnamen um Konsistenz!P. Pohl  !     *
      *        !zw. KFZ / REOB und DB/2 herzustellen  !         !     *
051004*AKI00387!Neues Feld Datum Erstzulassung VN     !W.Morfeld!     *
      *        !Änderung Filler im AMV-VERTR-KFZ      !         !     *
AKI553*AKI00553 Neue Felder f. Bestandsdatenerwei-    !W.Misgeld!     *
      *        !terung Kfz-Daten                      !         !     *
      *        !                                      !         !     *
200807*AAN00753! PIN-Nr. Erweit. auf 9 Stellen        !Hübner   !     *
      *        !                                      !         !     *
150908*AKI00731! Bestandsdatenerw. Kfz-Tarifierung    !W.Misgeld!     *
      *        !                                      !         !     *
260309*AKI00805!AMV-KFZ-VBEDINGUNG in 2 Felder aufget.!WMI      !     *
      *        !                                      !         !     *
300910*AKI00995! Dynamische Anzeige 3 neue Felder     !Hübner   !     *
      *        !                                      !         !     *
120711*AKI01041! Erweiterung d. Leistungskennzeichen  !WMI      !     *
      *        !                                      !         !     *
      *AKI01131! Neue Felder f. SFR, Halter, begl.    !BST      !     *
      *        ! Fahren, Umbau Datensatz              !         !     *
      *        !                                      !         !     *
300915*AKI01381! Neue Felder f. Moped Paket-Typ -Text !HHU      !     *
      *        !                                      !         !     *
210416*AKI01427! Neue Felder f. den NGT 2016          !tfl      !     *
      *        !                                      !         !     *
250816*AKI01465! VORT-Felder entfernen                !DDY      !     *
      *        !                                      !         !     *
030517*AKI01517! DTA überarbeiten                     !HHU      !
      *        !                                      !         !
      *AKI1779 ! neue Felder Firmen-Nr E-FTG-KZ       !HHU      !
      *        !                                      !         !     *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10064  !30.05.22! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    AMV-KFZ-GEBDAT-VN         !          
      * -----------!--------!------------------------------!----------
      * DBO-10494  !17.10.22! Erweiterung der Tabelle Zu-  ! T13EX78
      *            !        ! schläge/Abschläge von 10 auf !
      *            !        ! 25 Segmente. Neue Satzlänge  ! 
      *            !        ! daher 3915 Byte              !
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      *****************************************************************
 
       01  DRKI0702.
 
           05  AMV-KFZ-VORSATZ.
      *
               10  AMV-KFZ-V-SA                  PIC  X(001).
      *            SATZART = 'A'
               10  AMV-KFZ-V-ABSENDER            PIC  X(003).
      *            ABSENDER = 'AMV'
               10  AMV-KFZ-V-DATUM               PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               10  AMV-KFZ-V-EMPFAENGER          PIC  X(004).
      *            EMPFAENGER = 'DVAG'
               10  AMV-KFZ-V-TEXT                PIC  X(016).
      *            INHALTSANGABE = 'K BESTAND '
               10  AMV-KFZ-V-KZ                  PIC  X(004).
      *            INHALTSANGABE = 'INIT' für Komplettbestand
      *                            'AEND' für Änderungsbestand
300915         10  FILLER                        PIC  X(3879).
      *            RESERVE
 
           05  AMV-KFZ-SATZ REDEFINES AMV-KFZ-VORSATZ.
 
               10  AMV-VERTR-ALLGEMEIN.
                   15  AMV-VALL-AEND             PIC  9(002).
                   15  AMV-VALL-VERTRNR          PIC  9(015).
200807             15  AMV-VALL-INT-VERTRNR      PIC  9(009).
                   15  AMV-VALL-PRUEF            PIC  9(002).
                   15  AMV-VALL-VBEGINN          PIC  9(008).
                   15  AMV-VALL-VABLAUF          PIC  9(008).
                   15  AMV-VALL-ZUSTAND          PIC  X(001).
                   15  AMV-VALL-ZAHLUNGSW        PIC  X(001).
                   15  AMV-VALL-BENDBEITRAG      PIC S9(008)V99.
                   15  AMV-VALL-ABSCHLVERM       PIC  9(007).
                   15  AMV-VALL-AENDDATUM        PIC  9(008).
                   15  AMV-VALL-LETZTEAENDD      PIC  9(008).
                   15  AMV-VALL-ZAHLUNGSART      PIC  X(001).
                   15  AMV-VALL-STORNODAT        PIC  9(008).
                   15  AMV-VALL-GEB-VERTR        PIC  X(001).
                   15  AMV-VALL-WAEHRUNG-KZ      PIC  X(003).
                   15  AMV-VALL-FIRMEN-NR        PIC  X(006).
                   15  FILLER                    PIC  X(035).
 
               10  AMV-VERTR-KFZ.
                   15  AMV-KFZ-ZSCHL             PIC  9(002).
                   15  AMV-KFZ-ZTEXT             PIC  X(003).
      ****             Inkassoart LSV Scoring
                   15  AMV-KFZ-LSV-SCOR          PIC  X(001).
      ****             Amtliches Kennzeichen
                   15  AMV-KFZ-AMTKZ             PIC  X(012).
                   15  AMV-KFZ-AKZART-KNR        PIC  X(001).
      ****             Saisonkennzeichen
                   15  AMV-KFZ-SAISON-KZ         PIC  X(001).
                   15  AMV-KFZ-KFZ-SAISON-BEG    PIC  9(002).
                   15  AMV-KFZ-KFZ-SAISON-ENDE   PIC  9(002).
      ****             Datum Zulassung
                   15  AMV-KFZ-ZULASSD           PIC  9(008).
051004             15  AMV-KFZ-EZDATVN           PIC  9(008).
      ****             Fahrzeug
                   15  AMV-KFZ-FZGART-KNR        PIC  9(003).
                   15  AMV-KFZ-VERWEND-KNR       PIC  9(003).
                   15  AMV-KFZ-NAEHSPEZ-KNR      PIC  9(003).
                   15  AMV-KFZ-WAGNISKZ          PIC  9(003).
                   15  AMV-KFZ-KFZ-WKZ-TEXT      PIC  X(025).
 
                   15  AMV-KFZ-HERST-NR          PIC  X(004).
                   15  AMV-KFZ-HERSTELLTEXT      PIC  X(025).
                   15  AMV-KFZ-TYP-MOD-TEXT      PIC  X(050).
      ****             TSN
                   15  AMV-KFZ-KZ-KFZ-TYP        PIC  X(003).
                   15  AMV-KFZ-FAHRZ-ID          PIC  X(020).
                   15  AMV-KFZ-KZ-LEIST          PIC  X(001).
                   15  AMV-KFZ-LEISTUNG          PIC S9(005)V99.
120711             15  AMV-KFZ-PLATZ             PIC S9(003).
120711             15  AMV-KFZ-GESGEW            PIC S9(007).
120711             15  AMV-KFZ-KW                PIC S9(004).
120711             15  AMV-KFZ-NUTZ              PIC S9(007).
                   15  AMV-KFZ-KFZ-NEUW          PIC  9(007).
                   15  AMV-KFZ-JAHR-FAHRLEIST    PIC  9(007).
300910             15  AMV-KM-STAND-AKT          PIC S9(7).
300910             15  AMV-KM-STAND-AKT-DATUM    PIC  9(8).
300915             15  AMV-KFZ-PAKET-TYP         PIC  9(004).
300915             15  AMV-KFZ-PAKET-TEXT        PIC  X(150).
                   15  AMV-KFZ-E-FZG-KZ          PIC  X.
030517             15  FILLER                    PIC  X(139).
 
030517         10  AMV-VERTR-PERSON.
                   15  AMV-KFZ-FAHRER            PIC  X(010).
      ****             Geburtsdaten
                   15  AMV-KFZ-GEBDAT-FAHRER     PIC  X(010).
                   15  AMV-KFZ-FALTERMAX         PIC S9(002).
                   15  AMV-KFZ-ALTERSGRP-KNR     PIC S9(004).
      ****             Begleitetes Fahren
                   15  AMV-KFZ-BEGL-FAHRER       PIC S9(002).
                   15  AMV-KFZ-BEGL-FAHRER-KTX   PIC  X(010).
      ****             Halter
                   15  AMV-KFZ-FZG-HALTER        PIC S9(002).
                   15  AMV-KFZ-FZG-HALTER-KTX    PIC  X(010).
                   15  AMV-KFZ-HALT-NNAME        PIC  X(040).
                   15  AMV-KFZ-HALT-VNAME        PIC  X(040).
                   15  AMV-KFZ-FZG-NUTZ          PIC  X(010).
                   15  AMV-KFZ-FZG-ABSTELL       PIC  X(010).
                   15  AMV-KFZ-AUFBAUART-KNR     PIC S9(004).
 
      ****             Kenntexte
                   15  AMV-KFZ-FAHRGEBIET        PIC  X(010).
                   15  AMV-KFZ-FUEHRSCH-ERWERB   PIC  X(010) .
                   15  AMV-KFZ-WOHNEIGENT        PIC  X(010).
                   15  AMV-KFZ-AG-BRANCHE        PIC  X(060).
      ****             TELEMATIK
210416             15  AMV-KFZ-TELEMATIK-ID      PIC  9(014).
210416             15  AMV-KFZ-SCOREWERT         PIC  9(003).
210416             15  AMV-KFZ-SCORE-DATUM       PIC  9(008).
210416             15  AMV-KFZ-GEBDAT-VN         PIC  X(010).
                   15  FILLER                    PIC  X(130).
 
030517         10  AMV-VERTR-DECKUNG.
                   15  AMV-KFZ-TARIFAUSWAHL      PIC  X(001).
>>                 15  AMV-KFZ-WERKSTBINDKZ      PIC  X(001).
                   15  AMV-KFZ-DA-KH             PIC  9(002).
                   15  AMV-KFZ-RKL-KH            PIC  X(004).
                   15  AMV-KFZ-TYPKL-KH          PIC  9(002).
                   15  AMV-KFZ-BEITRAG-ZW-KH     PIC S9(008)V99.
                   15  AMV-KFZ-DA-KF             PIC  9(002).
                   15  AMV-KFZ-RKL-KF            PIC  X(004).
                   15  AMV-KFZ-TYPKL-KF          PIC  9(002).
                   15  AMV-KFZ-SB-VK             PIC S9(008)V99.
                   15  AMV-KFZ-SB-TK             PIC S9(008)V99.
                   15  AMV-KFZ-BEITR-ZW-KF       PIC S9(008)V99.
150908             15  AMV-KFZ-LEASING           PIC S9(001).
                   15  AMV-KFZ-SCHUTZBRIEF       PIC  X(010).
      ****             Schadensfreiheitsrabatte
                   15  AMV-KFZ-SFR-TAB.
                     20  AMV-KFZ-SFR     OCCURS 2.
                       25  AMV-KFZ-SFR-ELEM        PIC  X(015).
                   15  AMV-KFZ-SFR-RD REDEFINES AMV-KFZ-SFR-TAB.
120711               20  AMV-KFZ-SFR-FREI-KH       PIC  X(001).
                     20  AMV-KFZ-SFANZKH           PIC  9(003).
                     20  AMV-KFZ-PSFANZKH          PIC S9(002).
                     20  AMV-KFZ-BEITRAGKH         PIC  9(003).
                     20  AMV-KFZ-BEITRAGKL-KH      PIC  X(006).
120711               20  AMV-KFZ-SFR-FREI-VK       PIC  X(001).
                     20  AMV-KFZ-SFANZVK           PIC  9(003).
                     20  AMV-KFZ-PSFANZVK          PIC S9(002).
                     20  AMV-KFZ-BEITRAGKF         PIC  9(003).
                     20  AMV-KFZ-BKL-VK.
                       25  AMV-KFZ-BKL-VK1       PIC  X(003).
                       25  AMV-KFZ-BKL-VK2       PIC  X(003).
 
                   15  AMV-KFZ-VERS-BED               OCCURS 3.
260309                 20  AMV-KFZ-VBEDINGUNG.
260309                     25  AMV-KFZ-VBEDINGUNG-KEY PIC  X(002).
260309                     25  AMV-KFZ-VBEDINGUNG-TXT PIC  X(019).
 
                   15  AMV-KFZ-TARIF-BEZ              OCCURS 3.
                       20  AMV-KFZ-TARIFB        PIC  X(007).
 
                   15  AMV-KFZ-KSMVERTRAG-TAB.
                       20  AMV-KFZ-KSMVERTRAG             OCCURS 4.
                           25  AMV-KFZ-TAR-BST-SPART PIC  X(006).
                           25  AMV-KFZ-TAR-BST-KTX   PIC  X(010).
                           25  AMV-KFZ-TAR-BST-TXT   PIC  X(050).
                           25  AMV-KFZ-TAR-BST-GDVKZ PIC  X(050).
 
                   15  AMV-KFZ-BES-VEREINBARUNG-TAB.
                       20  AMV-KFZ-BES-VEREINBARUNG  OCCURS 15.
                           25  AMV-KFZ-BESVEREINB    PIC  X(050).
 
210416             15  FILLER                    PIC  X(140).
 
030517         10  AMV-VERTR-SONDERAUS.
                   15  AMV-KFZ-SONDERAUSSTATT       OCCURS 20.
                       20 AMV-KFZ-SONTEILNR-KNR  PIC S9(003).
                       20 AMV-KFZ-SONTEILEWERT   PIC S9(007)V99.
                   15  FILLER                    PIC  X(140).
 
030517         10  AMV-VERTR-ZUSCHL-NACHL.
                   15  AMV-VALL-TABELLE               OCCURS 25.
                       20  AMV-VALL-ZN-TEXT      PIC  X(026).
250403                 20  AMV-VALL-EINHEIT      PIC  9(001).
250403                 20  AMV-VALL-WERT         PIC S9(008)V99.
 
           05  AMV-KFZ-NACHSATZ REDEFINES AMV-KFZ-VORSATZ.
      *
               10  AMV-KFZ-N-SA                  PIC X(001).
      *            SATZART = 'E'
               10  AMV-KFZ-N-SATZANZAHL          PIC 9(009).
300915         10  FILLER                        PIC X(3905).
      *            RESERVE
 
      ******************************************************************
      *          E n d e    Copy-Member DRKI0702                       *
      ******************************************************************