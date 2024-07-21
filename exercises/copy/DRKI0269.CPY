      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KFZ-DATEN VERSICHERUNGEN KOMPLETTBESTAND    *
      *    --------------------------------------------------------    *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    SATZLÄNGE   1630                                            *
AKI553*    SATZLÄNGE   3000                                            *
300915*    SATZLÄNGE   3300                                            *
030517*    SATZLÄNGE   3500                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *21.10.98!NEUERSTELLUNG                           !B.GOLÜCKE! 01 *
      *        !                                        !         !    *
250403*AKI00338!Änderungen von Feldnamen um Konsistenz  !P. Pohl  !    *
      *        !zw. KFZ / REOB und DB/2 herzustellen    !         !    *
080503*AKI00338!KOMPL-VALL-TABELLE neu eingefügt        !P. Pohl  !    *
051004*AKI00387!Neues Feld Datum Erstzulassung VN       !W.Morfeld!    *
AKI553*AKI00553 Neue Felder f. Bestandsdatenerwei-      !W.Misgeld!    *
      *        !terung Kfz-Daten                        !         !    *
      *        !                                        !         !    *
200807*AAN00753! PIN-Nr. Erweit. auf 9 Stellen          !Hübner   !    *
      *        !                                        !         !    *
150908*AKI00731! Bestandsdatenerw. Kfz-Tarifierung      !W.Misgeld!    *
      *        !                                        !         !    *
260309*AKI00805!KOMPL-KFZ-VBEDINGUNG in 2 Felder aufget.!WMI      !    *
      *        !                                        !         !    *
300910*AKI00995! Dynamische Anzeige 3 neue Felder       !Hübner   !    *
      *        !                                        !         !    *
120711*AKI01041! Erweiterung d. Leistungskennzeichen    !WMI      !    *
      *        !                                        !         !    *
      *AKI01131! Neue Felder f. SFR, Halter, begl.      !BST      !    *
      *        ! Fahren, Umbau Datensatz                !         !    *
      *        !                                        !         !    *
300915*AKI01381! Neue Felder f. Moped Paket-Typ -Text   ! HHU     !    *
      *        !                                        !         !    *
210416*AKI01427! Neue Felder f. NGT 2016                ! tfl     !    *
      *        !                                        !         !    *
030517*AKI01517! DTA überarbeiten                       ! HHU     !
      *        !                                        !         !
      *AKI01779! neue Felder FIRMEN-NR E-FZG-KZ         ! HHU     !
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10064  !31.05.22! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    KOMPL-KFZ-GEBDAT-VN       !          
      * -----------!--------!------------------------------!----------
      * DBO-10494  !17.10.22! Erweiterung Tabelle Zuschl./ ! T13EX78
      *            !        ! Nachlässe                    !
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      *****************************************************************
      *
       01  DRKI0269.

         03  KOMPL-VERTRAG.

           05  KOMPL-VERTR-ALLGEMEIN.
               10  KOMPL-VALL-AEND           PIC  9(002).

               10  KOMPL-VALL-KEY.
                 15 KOMPL-VALL-VERTRNR       PIC  9(015).

200807         10  KOMPL-VALL-INT-VERTRNR    PIC  9(009).
               10  KOMPL-VALL-PRUEF          PIC  9(002).
               10  KOMPL-VALL-VBEGINN        PIC  9(008).
               10  KOMPL-VALL-VABLAUF        PIC  9(008).
               10  KOMPL-VALL-ZUSTAND        PIC  X(001).
               10  KOMPL-VALL-ZAHLUNGSW      PIC  X(001).
               10  KOMPL-VALL-BENDBEITRAG    PIC S9(008)V99.
               10  KOMPL-VALL-ABSCHLVERM     PIC  9(007).
               10  KOMPL-VALL-AENDDATUM      PIC  9(008).
               10  KOMPL-VALL-LETZTEAENDD    PIC  9(008).
               10  KOMPL-VALL-ZAHLUNGSART    PIC  X(001).
               10  KOMPL-VALL-STORNODAT      PIC  9(008).
               10  KOMPL-VALL-GEB-VERTR      PIC  X(001).
               10  KOMPL-VALL-WAEHRUNG-KZ    PIC  X(003).
               10  KOMPL-VALL-FIRMEN-NR       PIC  X(06).
               10  FILLER                    PIC  X(035).

           05  KOMPL-VERTR-KFZ.
               10  KOMPL-KFZ-ZSCHL             PIC  9(002).
               10  KOMPL-KFZ-ZTEXT             PIC  X(003).
      ****     LSV-SCOR    Inkassoart LSV Scoring
               10  KOMPL-KFZ-LSV-SCOR          PIC  X(001).
               10  KOMPL-KFZ-AMTKZ             PIC  X(012).
               10  KOMPL-KFZ-AKZART-KNR        PIC  X(001).
               10  KOMPL-KFZ-SAISON-KZ         PIC  X(001).
               10  KOMPL-KFZ-KFZ-SAISON-BEG    PIC  9(002).
               10  KOMPL-KFZ-KFZ-SAISON-ENDE   PIC  9(002).
               10  KOMPL-KFZ-ZULASSD           PIC  9(008).
051004         10  KOMPL-KFZ-EZDATVN           PIC  9(008).
               10  KOMPL-KFZ-FZGART-KNR        PIC  9(003).
               10  KOMPL-KFZ-VERWEND-KNR       PIC  9(003).
               10  KOMPL-KFZ-NAEHSPEZ-KNR      PIC  9(003).
               10  KOMPL-KFZ-WAGNISKZ          PIC  9(003).
               10  KOMPL-KFZ-KFZ-WKZ-TEXT      PIC  X(025).
               10  KOMPL-KFZ-HERST-NR          PIC  X(004).
               10  KOMPL-KFZ-HERSTELLTEXT      PIC  X(025).
               10  KOMPL-KFZ-TYP-MOD-TEXT      PIC  X(050).
               10  KOMPL-KFZ-KZ-KFZ-TYP        PIC  X(003).
               10  KOMPL-KFZ-FAHRZ-ID          PIC  X(020).
               10  KOMPL-KFZ-KZ-LEIST          PIC  X(001).
               10  KOMPL-KFZ-LEISTUNG          PIC S9(005)V99.
120711         10  KOMPL-KFZ-PLATZ             PIC S9(003).
120711         10  KOMPL-KFZ-GESGEW            PIC S9(007).
120711         10  KOMPL-KFZ-KW                PIC S9(004).
120711         10  KOMPL-KFZ-NUTZ              PIC S9(007).
               10  KOMPL-KFZ-KFZ-NEUW          PIC  9(007).
               10  KOMPL-KFZ-JAHR-FAHRLEIST    PIC  9(007).
300910         10  KOMPL-KM-STAND-AKT          PIC S9(7).
300910         10  KOMPL-KM-STAND-AKT-DATUM    PIC 9(8).
               10  KOMPL-KFZ-PAKET-TYP         PIC 9(04).
               10  KOMPL-KFZ-PAKET-TEXT        PIC X(150).
               10  KOMPL-KFZ-E-FZG-KZ          PIC X(01).
030517         10  FILLER                      PIC X(139).

030517     05  KOMPL-VERTR-PERSON.
               10  KOMPL-KFZ-FAHRER            PIC  X(010).
      *            Geburtsdaten
               10  KOMPL-KFZ-GEBDAT-FAHRER     PIC  X(010).
               10  KOMPL-KFZ-FALTERMAX         PIC S9(002).
               10  KOMPL-KFZ-ALTERSGRP-KNR     PIC S9(004).
               10  KOMPL-KFZ-BEGL-FAHRER       PIC S9(002).
               10  KOMPL-KFZ-BEGL-FAHRER-KTX   PIC  X(010).
               10  KOMPL-KFZ-FZG-HALTER        PIC S9(002).
               10  KOMPL-KFZ-FZG-HALTER-KTX    PIC  X(010).
      *            abweichender Halter-Name
               10  KOMPL-KFZ-HALT-NNAME        PIC  X(040).
               10  KOMPL-KFZ-HALT-VNAME        PIC  X(040).
      *            sonstige Vertragsangaben
               10  KOMPL-KFZ-FZG-NUTZ          PIC  X(010).
               10  KOMPL-KFZ-FZG-ABSTELL       PIC  X(010).
               10  KOMPL-KFZ-AUFBAUART-KNR     PIC S9(004).
      *            Kenntexte
               10  KOMPL-KFZ-FAHRGEBIET        PIC  X(010).
               10  KOMPL-KFZ-FUEHRSCH-ERWERB   PIC  X(010) .
               10  KOMPL-KFZ-WOHNEIGENT        PIC  X(010).
               10  KOMPL-KFZ-AG-BRANCHE        PIC  X(060).
210416         10  KOMPL-KFZ-TELEMATIK-ID          PIC 9(014).
210416         10  KOMPL-KFZ-SCOREWERT             PIC 9(003).
210416         10  KOMPL-KFZ-SCORE-DATUM           PIC 9(008).
               10  KOMPL-KFZ-GEBDAT-VN         PIC  X(010).
               10  FILLER                      PIC  X(130).

030517     05  KOMPL-VERTR-DECKUNG.
               10  KOMPL-KFZ-TARIFAUSWAHL      PIC  X(001).
               10  KOMPL-KFZ-WERKSTBINDKZ      PIC  X(001).
               10  KOMPL-KFZ-DA-KH             PIC  9(002).
               10  KOMPL-KFZ-RKL-KH            PIC  X(004).
               10  KOMPL-KFZ-TYPKL-KH          PIC  9(002).
               10  KOMPL-KFZ-BEITRAG-ZW-KH     PIC S9(008)V99.
               10  KOMPL-KFZ-DA-KF             PIC  9(002).
               10  KOMPL-KFZ-RKL-KF            PIC  X(004).
               10  KOMPL-KFZ-TYPKL-KF          PIC  9(002).
               10  KOMPL-KFZ-SB-VK             PIC S9(008)V99.
               10  KOMPL-KFZ-SB-TK             PIC S9(008)V99.
               10  KOMPL-KFZ-BEITR-ZW-KF       PIC S9(008)V99.
150908         10  KOMPL-KFZ-LEASING           PIC S9(001).
               10  KOMPL-KFZ-SCHUTZBRIEF       PIC  X(010).

               10  KOMPL-KFZ-SFR.
120711            15  KOMPL-KFZ-SFR-FREI-KH    PIC  X(001).
                  15  KOMPL-KFZ-SFANZKH        PIC  9(003).
                  15  KOMPL-KFZ-PSFANZKH       PIC S9(002).
                  15  KOMPL-KFZ-BEITRAGKH      PIC  9(003).
                  15  KOMPL-KFZ-BEITRAGKL-KH   PIC  X(006).
120711            15  KOMPL-KFZ-SFR-FREI-VK    PIC  X(001).
                  15  KOMPL-KFZ-SFANZVK        PIC  9(003).
                  15  KOMPL-KFZ-PSFANZVK       PIC S9(002).
                  15  KOMPL-KFZ-BEITRAGKF      PIC  9(003).
                  15  KOMPL-KFZ-BKL-VK.
                    20 KOMPL-KFZ-BKL-VK1       PIC  X(003).
                    20 KOMPL-KFZ-BKL-VK2       PIC  X(003).

               10  KOMPL-KFZ-KOMPL-BED       OCCURS 3.
260309             20  KOMPL-KFZ-VBEDINGUNG.
260309                 25  KOMPL-KFZ-VBEDINGUNG-KEY  PIC  X(002).
260309                 25  KOMPL-KFZ-VBEDINGUNG-TXT  PIC  X(019).
               10  KOMPL-KFZ-TARIF-BEZ       OCCURS 3.
                   20  KOMPL-KFZ-TARIFB      PIC  X(007).

      ****         Tarifbestimmung
               10  KOMPL-KFZ-KSMVERTRAG-TAB.
                   15  KOMPL-KFZ-KSMVERTRAG        OCCURS 4.
                       20  KOMPL-KFZ-TAR-BST-SPART PIC  X(006).
                       20  KOMPL-KFZ-TAR-BST-KTX   PIC  X(010).
                       20  KOMPL-KFZ-TAR-BST-TXT   PIC  X(050).
                       20  KOMPL-KFZ-TAR-BST-GDVKZ PIC  X(050).
               10  KOMPL-KFZ-BES-VEREINBARUNG-TAB.
                   15  KOMPL-KFZ-BES-VEREINBARUNG  OCCURS 15.
                       20  KOMPL-KFZ-BESVEREINB    PIC  X(050).

221215         10  FILLER                          PIC X(140).

030517     05  KOMPL-VERTR-SONDERAUS.
               10  KOMPL-KFZ-SONDERAUSSTATT       OCCURS 20.
                   15 KOMPL-KFZ-SONTEILNR-KNR  PIC S9(003).
                   15 KOMPL-KFZ-SONTEILEWERT   PIC S9(007)V99.
               10  FILLER                      PIC X(140).

030517     05  KOMPL-VERTR-ZUSCHL-NACHL.
080503         10  KOMPL-VALL-TABELLE.
080503           15  KOMPL-VALL-TAB          OCCURS 25.
                   20  KOMPL-VALL-ZN-TEXT    PIC  X(026).
250403             20  KOMPL-VALL-Einheit    PIC  9(001).
250403             20  KOMPL-VALL-Wert       PIC S9(008)V99.
030517         10  FILLER                    PIC X(131).
210416* Diese Felder stehen ABSICHTLICH hinter dem Filler, da sie mit
210416* standardmäßigen Belieferung nichts zu tun haben. (HHU)
030517     05  KOMPL-VERTR-COC.
221215         10  KOMPL-COC-DATUM                 PIC 9(8).
221215         10  KOMPL-COC-KZ                    PIC X.

      ******************************************************************
      *          E n d e    Copy-Member DRKI0269                       *
      ******************************************************************


