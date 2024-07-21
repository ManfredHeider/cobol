      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0375.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0375.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   18 Feb 1999 13:05:52   I13PC16
      *Komplettbestand AMV Savari
      *
      *   Rev 1.0   14 Jul 1998 17:03:12   P13SP13
      *Initial revision.
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR SAVARI-DATEN AMV KOMPLETTBESTAND            *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    SATZLÄNGE 11711                                             *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *21.10.98!NEUERSTELLUNG                           !B.GOLÜCKE! 01 *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0375.

          05  KOMPL-S-BESTAND.
      *
      *  Allgemeine Daten
      *
              10  KOMPL-S-ALLG-DATEN.
                  15  KOMPL-S-A-AEND-KZ                  PIC 9(002).
                  15  KOMPL-S-A-KEY.
                      20  KOMPL-S-A-VSNR                 PIC 9(015).
                  15  KOMPL-S-A-DVAG-VSNR                PIC 9(007).
                  15  KOMPL-S-A-DVAG-PRUEF               PIC 9(002).
                  15  KOMPL-S-A-BEGINN                   PIC 9(008).
                  15  FILLER REDEFINES KOMPL-S-A-BEGINN.
                      20  KOMPL-S-A-BEG-JJJJ             PIC 9(004).
                      20  KOMPL-S-A-BEG-MM               PIC 9(002).
                      20  KOMPL-S-A-BEG-TT               PIC 9(002).
                  15  FILLER REDEFINES KOMPL-S-A-BEGINN.
                      20  KOMPL-S-A-BEG-JH               PIC 9(002).
                      20  KOMPL-S-A-BEG-JJ               PIC 9(002).
                      20  FILLER                         PIC 9(004).
                  15  KOMPL-S-A-ABLAUF                   PIC 9(008).
                  15  FILLER REDEFINES KOMPL-S-A-ABLAUF.
                      20  KOMPL-S-A-ABL-JJJJ             PIC 9(004).
                      20  KOMPL-S-A-ABL-MM               PIC 9(002).
                      20  KOMPL-S-A-ABL-TT               PIC 9(002).
                  15  FILLER REDEFINES KOMPL-S-A-ABLAUF.
                      20  KOMPL-S-A-ABL-JH               PIC 9(002).
                      20  KOMPL-S-A-ABL-JJ               PIC 9(002).
                      20  FILLER                         PIC 9(004).
                  15  KOMPL-S-A-VERZUSTAND               PIC X(001).
                  15  KOMPL-S-A-BLZ                      PIC 9(011).
                  15  KOMPL-S-A-KTONR                    PIC 9(018).
                  15  KOMPL-S-A-ZW                       PIC X(001).
                  15  KOMPL-S-A-BEITRAG                  PIC 9(008)V99.
                  15  KOMPL-S-A-DVAG-VBNR                PIC 9(007).
                  15  KOMPL-S-A-AENDBEG                  PIC 9(008).
                  15  FILLER REDEFINES KOMPL-S-A-AENDBEG.
                      20  KOMPL-S-A-AEND-JJJJ            PIC 9(004).
                      20  KOMPL-S-A-AEND-MM              PIC 9(002).
                      20  KOMPL-S-A-AEND-TT              PIC 9(002).
                  15  FILLER REDEFINES KOMPL-S-A-AENDBEG.
                      20  KOMPL-S-A-AEND-JH              PIC 9(002).
                      20  KOMPL-S-A-AEND-JJ              PIC 9(002).
                      20  FILLER                         PIC 9(004).
                  15  KOMPL-S-A-LBDAT                    PIC 9(008).
                  15  FILLER REDEFINES KOMPL-S-A-LBDAT.
                      20  KOMPL-S-A-LBDAT-JJJJ           PIC 9(004).
                      20  KOMPL-S-A-LBDAT-MM             PIC 9(002).
                      20  KOMPL-S-A-LBDAT-TT             PIC 9(002).
                  15  FILLER REDEFINES KOMPL-S-A-LBDAT.
                      20  KOMPL-S-A-LBDAT-JH             PIC 9(002).
                      20  KOMPL-S-A-LBDAT-JJ             PIC 9(002).
                      20  FILLER                         PIC 9(004).
                  15  KOMPL-S-A-LBUHRZEIT                PIC 9(008).
                  15  KOMPL-S-A-INKASSO                  PIC X(001).
                  15  KOMPL-S-A-STORNO                   PIC 9(008).
                  15  FILLER REDEFINES KOMPL-S-A-STORNO.
                      20  KOMPL-S-A-STORNO-JJJJ          PIC 9(004).
                      20  KOMPL-S-A-STORNO-MM            PIC 9(002).
                      20  KOMPL-S-A-STORNO-TT            PIC 9(002).
                  15  FILLER REDEFINES KOMPL-S-A-STORNO.
                      20  KOMPL-S-A-STORNO-JH            PIC 9(002).
                      20  KOMPL-S-A-STORNO-JJ            PIC 9(002).
                      20  FILLER                         PIC 9(004).
                  15  KOMPL-S-A-NACHL-TABELLE.
                      20  KOMPL-S-A-NACHL-TABELLE-SAV OCCURS 10.
                          25  KOMPL-S-A-NACHL-TEXT       PIC X(026).
                          25  KOMPL-S-A-NACHL-EINHEIT    PIC 9(001).
                          25  KOMPL-S-A-NACHL-WERT       PIC 9(008)V99.
                  15  KOMPL-S-A-GEB-VERTRAG              PIC X(001).
                  15  KOMPL-S-A-VORT-LKZ                 PIC X(003).
                  15  KOMPL-S-A-VORT-PLZ                 PIC X(005).
                  15  KOMPL-S-A-VORT-STR                 PIC X(021).
                  15  KOMPL-S-A-VORT-ORT                 PIC X(021).
                  15  KOMPL-S-A-WAEHRUNG-KZ              PIC X(003).
                  15  KOMPL-S-A-FILLER                   PIC X(320).
      *
      *  Daten aus SAVARI
      *
               10  KOMPL-S-SACH-DATEN.
                   15  KOMPL-S-SACH-WOHNGEB.
                       20  KOMPL-S-SW-ZWG                PIC 9(002).
                       20  KOMPL-S-SW-ZWG-TEXT           PIC X(003).
                       20  KOMPL-S-SW-GEFAHR-ZONE-SB           OCCURS 5.
                           25  KOMPL-S-SW-GEFAHR         PIC X(003).
                           25  KOMPL-S-SW-ZONE           PIC 9(001).
                       20  KOMPL-S-SW-ST-SB.
                           25  KOMPL-S-SW-ST-VSUMME      PIC 9(002).
                           25  KOMPL-S-SW-ST-SCHADEN     PIC 9(002).
                           25  KOMPL-S-SW-ST-MIN         PIC 9(005).
                           25  KOMPL-S-SW-ST-MAX         PIC 9(005).
                       20  KOMPL-S-SW-LW-SB.
                           25  KOMPL-S-SW-LW-VSUMME      PIC 9(002).
                           25  KOMPL-S-SW-LW-SCHADEN     PIC 9(002).
                           25  KOMPL-S-SW-LW-MIN         PIC 9(005).
                           25  KOMPL-S-SW-LW-MAX         PIC 9(005).
                       20  KOMPL-S-SW-EL-ERD-SB.
                           25  KOMPL-S-SW-EL-ERD-VSUMME  PIC 9(002).
                           25  KOMPL-S-SW-EL-ERD-SCHADEN PIC 9(002).
                           25  KOMPL-S-SW-EL-ERD-MIN     PIC 9(005).
                           25  KOMPL-S-SW-EL-ERD-MAX     PIC 9(005).
                       20  KOMPL-S-SW-EL-SON-SB.
                           25  KOMPL-S-SW-EL-SON-VSUMME  PIC 9(002).
                           25  KOMPL-S-SW-EL-SON-SCHADEN PIC 9(002).
                           25  KOMPL-S-SW-EL-SON-MIN     PIC 9(005).
                           25  KOMPL-S-SW-EL-SON-MAX     PIC 9(005).
                       20  KOMPL-S-SW-VSUMME             PIC 9(009)V99.
                       20  KOMPL-S-SW-SB-KZ              PIC X(002).
                       20  KOMPL-S-SW-LEISTUNG           PIC 9(002).
                       20  KOMPL-S-SW-BAUART-KLASSE      PIC X(002).
                       20  KOMPL-S-SW-BAUJAHR            PIC 9(004).
                       20  KOMPL-S-SW-ART-GEBAEUDE       PIC 9(002).
                       20  KOMPL-S-SW-GN-FAKTOR          PIC 9(002)V9.
                       20  KOMPL-S-SW-VGB88-GARAGE       PIC X(001).
                       20  KOMPL-S-SW-VGB88-NEBENGEB     PIC X(001).
                       20  KOMPL-S-SW-VGB88-ANBAU        PIC X(001).
                       20  KOMPL-S-SW-VGB88-CARPORT      PIC X(001).
                       20  KOMPL-S-SW-UMFANG       PIC 9(003) OCCURS 20.
                       20  KOMPL-S-SW-BEDING       PIC 9(002) OCCURS 15.
                       20  KOMPL-S-SW-KLAUSEL      PIC 9(004) OCCURS 15.
                       20  KOMPL-S-SW-NETTO-PRAEM        PIC 9(008)V99.
                       20  KOMPL-S-SW-FILLER             PIC X(100).
     *
                   15  KOMPL-S-SACH-HAUSRAT.
                       20  KOMPL-S-SH-ZWG               PIC 9(002).
                       20  KOMPL-S-SH-ZWG-TEXT          PIC X(003).
                       20  KOMPL-S-SH-GEFAHR        PIC X(003) OCCURS 6.
                       20  KOMPL-S-SH-VSUMME            PIC 9(009)V99.
                       20  KOMPL-S-SH-SB-KZ             PIC X(002).
                       20  KOMPL-S-SH-LEISTUNG          PIC 9(002).
                       20  KOMPL-S-SH-ZONE              PIC 9(001).
                       20  KOMPL-S-SH-ART-GEBAEUDE      PIC 9(001).
                       20  KOMPL-S-SH-BEWOHNT           PIC X(001).
                       20  KOMPL-S-SH-QM-WOHNFL         PIC 9(005).
                       20  KOMPL-S-SH-UMFANG-PROZ             OCCURS 5.
                           25  KOMPL-S-SH-UMFANG         PIC 9(003).
                           25  KOMPL-S-SH-PROZ           PIC 9(002)V9.
                       20  KOMPL-S-SH-BEDING       PIC 9(002) OCCURS 15.
                       20  KOMPL-S-SH-KLAUSEL      PIC 9(004) OCCURS 15.
                       20  KOMPL-S-SH-PRAEMIE           PIC 9(005)V99.
                       20  KOMPL-S-SH-FILLER            PIC X(100).
     *
                   15  KOMPL-S-SACH-GLAS.
                       20  KOMPL-S-SG-ZWG               PIC 9(002).
                       20  KOMPL-S-SG-ZWG-TEXT          PIC X(003).
                       20  KOMPL-S-SG-WOHNFLAECHE       PIC 9(005).
                       20  KOMPL-S-SG-GEFAHR-INFO             OCCURS 10.
                           25  KOMPL-S-SG-GEFAHR        PIC 9(003).
                           25  KOMPL-S-SG-BERECH-NACH   PIC 9(002).
                           25  KOMPL-S-SG-BERECH-WERT   PIC 9(009).
                       20  KOMPL-S-SG-ART-GEBAEUDE      PIC 9(001).
                       20  KOMPL-S-SG-BEDING       PIC 9(002) OCCURS 15.
                       20  KOMPL-S-SG-KLAUSEL      PIC 9(004) OCCURS 15.
                       20  KOMPL-S-SG-PRAEMIE           PIC 9(005)V99.
                       20  KOMPL-S-SG-FILLER            PIC X(100).
     *
                   15  KOMPL-S-HAFTPFLICHT.
                       20  KOMPL-S-H-ZWG                PIC 9(002).
                       20  KOMPL-S-H-ZWG-TEXT           PIC X(003).
                       20  KOMPL-S-H-BEDING        PIC 9(002) OCCURS 15.
                       20  KOMPL-S-H-KLAUSEL       PIC 9(004) OCCURS 15.
                       20  KOMPL-S-H-PRODUKT            PIC 9(002).
                       20  KOMPL-S-H-PRODUKT-GRUND-ZUS        OCCURS 2.
                           25  KOMPL-S-H-GRUND-ZUS-KZ    PIC X(001).
                           25  KOMPL-S-H-WANR-MENGE           OCCURS 20.
                               30  KOMPL-S-H-WANR        PIC 9(004).
                               30  KOMPL-S-H-MENGE       PIC 9(007).
                               30  KOMPL-S-H-MENGE-TEXT  PIC X(006).
                           25  KOMPL-S-H-VSUM-PERS       PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-SACH       PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-VERM       PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-EK         PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-MIET       PIC 9(008)V99.
                           25  KOMPL-S-H-PRAEMIE         PIC 9(005)V99.
                           25  KOMPL-S-H-VSUM-PERS-SB    PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-SACH-SB    PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-VERM-SB    PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-EK-SB      PIC 9(008)V99.
                           25  KOMPL-S-H-VSUM-MIET-SB    PIC 9(008)V99.
                           25  KOMPL-S-H-LEBENSPARTNER   PIC X(021).
                       20  KOMPL-S-H-FILLER              PIC X(90).
     *
                   15  KOMPL-S-UNFALL.
                       20  KOMPL-S-U-ZWG                 PIC 9(002).
                       20  KOMPL-S-U-ZWG-TEXT            PIC X(003).
                       20  KOMPL-S-U-BEDING        PIC 9(002) OCCURS 15.
                       20  KOMPL-S-U-KLAUSEL       PIC 9(004) OCCURS 15.
                       20  KOMPL-S-U-DTUTAB OCCURS 33.
                           25  KOMPL-S-U-PROGRESSION     PIC 9(004).
                           25  KOMPL-S-U-DYNAMIKPROZ     PIC 9(002)V99.
                           25  KOMPL-S-U-NAME            PIC X(021).
                           25  KOMPL-S-U-GEBDAT          PIC X(008).
                           25  FILLER REDEFINES KOMPL-S-U-GEBDAT.
                               30  FILLER                PIC 9(002).
                               30  KOMPL-S-U-GEBDAT-JMT  PIC 9(006).
                           25  FILLER REDEFINES KOMPL-S-U-GEBDAT.
                               30  KOMPL-S-U-GEBDAT-JH   PIC 9(002).
                               30  KOMPL-S-U-GEBDAT-JJ   PIC 9(002).
                               30  FILLER                PIC 9(004).
                           25  KOMPL-S-U-BERUF           PIC 9(003).
                           25  KOMPL-S-U-BERUF-GEN       PIC 9(001).
                           25  KOMPL-S-U-DECKUNG         PIC 9(001).
                           25  KOMPL-S-U-VSUM-TOD        PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-INV        PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-KTG        PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-TG         PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-GG         PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-HEIL       PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-HHILF      PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-SCHULGE    PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-GRUNDSUM   PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-UEBERG     PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-KOSMETIK   PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-BERGUNG    PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-KUR        PIC 9(008)V99.
                           25  KOMPL-S-U-VSUM-RENTE      PIC 9(008)V99.
                           25  KOMPL-S-U-PRAEMIE         PIC 9(005)V99.
                       20  KOMPL-S-U-FILLER              PIC X(100).
     *
                   15  KOMPL-S-SONSTIGE-SACH.
                       20  KOMPL-S-SONSTIGE-SACH-DATEN       OCCURS 9.
                           25  KOMPL-S-SS-LFDNR          PIC 9(002).
                           25  KOMPL-S-SS-ZWG            PIC 9(002).
                           25  KOMPL-S-SS-ZWG-TEXT       PIC X(003).
                           25  KOMPL-S-SS-VSUMME         PIC 9(009)V99.
                           25  KOMPL-S-SS-bedocc             OCCURS 15.
                               30  KOMPL-S-SS-BEDING     PIC 9(002).
                           25  KOMPL-S-SS-KLAOCC             OCCURS 15.
                               30  KOMPL-S-SS-KLAUSEL    PIC 9(004).
                           25  KOMPL-S-SS-PRAEMIE        PIC 9(005)V99.
                           25  KOMPL-S-SS-FILLER         PIC X(100).
                   15 FILLER                             PIC X(360).


      ******************************************************************
      *          E n d e    Copy-Member DRKI0375                       *
      ******************************************************************
