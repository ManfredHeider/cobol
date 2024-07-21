      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0955.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRKI0955.CPv  $
      *
      *   Rev 2.3   28 Sep 2000 10:52:16   T13EX41
      *AKI00064
      *- Umstellung SAVARI auf Satzarten
      *
      *   Rev 2.2   May 08 2000 14:20:52   T13EX41
      *AKI00064
      *- Hausrat und Glas geändert
      *
      *   Rev 2.1   Feb 18 2000 13:58:38   T13EX50
      *AKI64 - Sicherung
      *
      *   Rev 2.0   Nov 29 1999 13:55:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - EINGABEDATEI AMV-SAVARI-DATEN KSDS                      *
      *                                                               *
      *    SATZLÄNGE   MIN 210 MAX 592                                *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *07.01.00! NEU                                  ! HERWEG  ! 2.0 *
      *AKI00064!                                      !         !     *
      *08.05.00! WEITERENTWICKLUNG                    !C.HEINECK!     *
      *AKI00064!                                      !         !     *
      *        !                                      !         !     *
      *****************************************************************

      *
      *  SATZART 100 - ALLGEMEINE DATEN
      *
       01 KOMPL-S-ALLG-DATEN.
          05  KOMPL-S-A-KEY.
              10  KOMPL-S-A-VSNR-KEY             PIC 9(015).
              10  KOMPL-S-A-SATZART-KEY          PIC 9(003).
              10  KOMPL-S-A-LFDNR-KEY            PIC 9(003).
          05  KOMPL-S-A-AEND-KZ                  PIC 9(002).
          05  KOMPL-S-A-VSNR                     PIC 9(015).
          05  KOMPL-S-A-DVAG-VSNR                PIC 9(007).
          05  KOMPL-S-A-DVAG-PRUEF               PIC 9(002).
          05  KOMPL-S-A-BEGINN                   PIC 9(008).
          05  FILLER REDEFINES KOMPL-S-A-BEGINN.
              10  KOMPL-S-A-BEG-JJJJ             PIC 9(004).
              10  KOMPL-S-A-BEG-MM               PIC 9(002).
              10  KOMPL-S-A-BEG-TT               PIC 9(002).
          05  FILLER REDEFINES KOMPL-S-A-BEGINN.
              10  KOMPL-S-A-BEG-JH               PIC 9(002).
              10  KOMPL-S-A-BEG-JJ               PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-A-ABLAUF                   PIC 9(008).
          05  FILLER REDEFINES KOMPL-S-A-ABLAUF.
              10  KOMPL-S-A-ABL-JJJJ             PIC 9(004).
              10  KOMPL-S-A-ABL-MM               PIC 9(002).
              10  KOMPL-S-A-ABL-TT               PIC 9(002).
          05  FILLER REDEFINES KOMPL-S-A-ABLAUF.
              10  KOMPL-S-A-ABL-JH               PIC 9(002).
              10  KOMPL-S-A-ABL-JJ               PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-A-VERZUSTAND               PIC X(001).
          05  KOMPL-S-A-BLZ                      PIC 9(011).
          05  KOMPL-S-A-KTONR                    PIC 9(018).
          05  KOMPL-S-A-ZW                       PIC X(001).
          05  KOMPL-S-A-BEITRAG                  PIC 9(008)V99.
          05  KOMPL-S-A-DVAG-VBNR                PIC 9(007).
          05  KOMPL-S-A-AENDBEG                  PIC 9(008).
          05  FILLER REDEFINES KOMPL-S-A-AENDBEG.
              10  KOMPL-S-A-AEND-JJJJ            PIC 9(004).
              10  KOMPL-S-A-AEND-MM              PIC 9(002).
              10  KOMPL-S-A-AEND-TT              PIC 9(002).
          05  FILLER REDEFINES KOMPL-S-A-AENDBEG.
              10  KOMPL-S-A-AEND-JH              PIC 9(002).
              10  KOMPL-S-A-AEND-JJ              PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-A-LBDAT                    PIC 9(008).
          05  FILLER REDEFINES KOMPL-S-A-LBDAT.
              10  KOMPL-S-A-LBDAT-JJJJ           PIC 9(004).
              10  KOMPL-S-A-LBDAT-MM             PIC 9(002).
              10  KOMPL-S-A-LBDAT-TT             PIC 9(002).
          05  FILLER REDEFINES KOMPL-S-A-LBDAT.
              10  KOMPL-S-A-LBDAT-JH             PIC 9(002).
              10  KOMPL-S-A-LBDAT-JJ             PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-A-LBUHRZEIT                PIC 9(008).
          05  KOMPL-S-A-INKASSO                  PIC X(001).
          05  KOMPL-S-A-STORNO                   PIC 9(008).
          05  FILLER REDEFINES KOMPL-S-A-STORNO.
              10  KOMPL-S-A-STORNO-JJJJ          PIC 9(004).
              10  KOMPL-S-A-STORNO-MM            PIC 9(002).
              10  KOMPL-S-A-STORNO-TT            PIC 9(002).
          05  FILLER REDEFINES KOMPL-S-A-STORNO.
              10  KOMPL-S-A-STORNO-JH            PIC 9(002).
              10  KOMPL-S-A-STORNO-JJ            PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-A-NACHL-TABELLE.
              10 KOMPL-S-A-NACHL-TABELLE-SAV OCCURS 10.
                 15 KOMPL-S-A-NACHL-TEXT         PIC X(026).
                 15 KOMPL-S-A-NACHL-EINHEIT      PIC 9(001).
                 15 KOMPL-S-A-NACHL-WERT         PIC 9(008)V99.
          05  KOMPL-S-A-GEB-VERTRAG              PIC X(001).
          05  KOMPL-S-A-VORT-LKZ                 PIC X(003).
          05  KOMPL-S-A-VORT-PLZ                 PIC X(005).
          05  KOMPL-S-A-VORT-STR                 PIC X(021).
          05  KOMPL-S-A-VORT-ORT                 PIC X(021).
          05  KOMPL-S-A-WAEHRUNG-KZ              PIC X(003).
      *
      *  SATZART 200 - WOHNGEBAEUDE
      *
       01 KOMPL-S-SACH-WOHNGEB.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-SW-ZWG                     PIC 9(002).
          05  KOMPL-S-SW-ZWG-TEXT                PIC X(003).
          05  KOMPL-S-SW-GEFAHR-ZONE-SB OCCURS 5.
              10  KOMPL-S-SW-GEFAHR              PIC X(003).
              10  KOMPL-S-SW-ZONE                PIC 9(001).
          05  KOMPL-S-SW-ST-SB.
              10  KOMPL-S-SW-ST-VSUMME           PIC 9(002).
              10  KOMPL-S-SW-ST-SCHADEN          PIC 9(002).
              10  KOMPL-S-SW-ST-MIN              PIC 9(005).
              10  KOMPL-S-SW-ST-MAX              PIC 9(005).
          05  KOMPL-S-SW-LW-SB.
              10  KOMPL-S-SW-LW-VSUMME           PIC 9(002).
              10  KOMPL-S-SW-LW-SCHADEN          PIC 9(002).
              10  KOMPL-S-SW-LW-MIN              PIC 9(005).
              10  KOMPL-S-SW-LW-MAX              PIC 9(005).
          05  KOMPL-S-SW-EL-ERD-SB.
              10  KOMPL-S-SW-EL-ERD-VSUMME       PIC 9(002).
              10  KOMPL-S-SW-EL-ERD-SCHADEN      PIC 9(002).
              10  KOMPL-S-SW-EL-ERD-MIN          PIC 9(005).
              10  KOMPL-S-SW-EL-ERD-MAX          PIC 9(005).
          05  KOMPL-S-SW-EL-SON-SB.
              10  KOMPL-S-SW-EL-SON-VSUMME       PIC 9(002).
              10  KOMPL-S-SW-EL-SON-SCHADEN      PIC 9(002).
              10  KOMPL-S-SW-EL-SON-MIN          PIC 9(005).
              10  KOMPL-S-SW-EL-SON-MAX          PIC 9(005).
          05  KOMPL-S-SW-VSUMME                  PIC 9(009)V99.
          05  KOMPL-S-SW-SB-KZ                   PIC X(002).
          05  KOMPL-S-SW-LEISTUNG                PIC 9(002).
          05  KOMPL-S-SW-BAUART-KLASSE           PIC X(002).
          05  KOMPL-S-SW-BAUJAHR                 PIC 9(004).
          05  KOMPL-S-SW-ART-GEBAEUDE            PIC 9(002).
          05  KOMPL-S-SW-GN-FAKTOR               PIC 9(002)V9.
          05  KOMPL-S-SW-VGB88-GARAGE            PIC X(001).
          05  KOMPL-S-SW-VGB88-NEBENGEB          PIC X(001).
          05  KOMPL-S-SW-VGB88-ANBAU             PIC X(001).
          05  KOMPL-S-SW-VGB88-CARPORT           PIC X(001).
          05  KOMPL-S-SW-UMFANG                  PIC 9(009) OCCURS 20.
          05  KOMPL-S-SW-BEDING                  PIC 9(009) OCCURS 15.
          05  KOMPL-S-SW-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  KOMPL-S-SW-NETTO-PRAEM             PIC 9(008)V99.

      *
      *  SATZART 300 - HAUSRAT
      *
       01 KOMPL-S-SACH-HAUSRAT.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-SH-ZWG                     PIC 9(002).
          05  KOMPL-S-SH-ZWG-TEXT                PIC X(003).
          05  KOMPL-S-SH-GEFAHR                  PIC X(003) OCCURS 6.
          05  KOMPL-S-SH-VSUMME                  PIC 9(009)V99.
          05  KOMPL-S-SH-SB-KZ                   PIC X(002).
          05  KOMPL-S-SH-LEISTUNG                PIC 9(002).
          05  KOMPL-S-SH-ZONE                    PIC 9(001).
          05  KOMPL-S-SH-ART-GEBAEUDE            PIC 9(001).
          05  KOMPL-S-SH-BEWOHNT                 PIC X(001).
          05  KOMPL-S-SH-QM-WOHNFL               PIC 9(005).
          05  KOMPL-S-SH-UMFANG-PROZ OCCURS  20.
              10  KOMPL-S-SH-UMFANG              PIC 9(009).
              10  KOMPL-S-SH-PROZ                PIC 9(002)V9.
          05  KOMPL-S-SH-BEDING                  PIC 9(009) OCCURS 15.
          05  KOMPL-S-SH-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  KOMPL-S-SH-PRAEMIE                 PIC 9(005)V99.

      *
      *  SATZART 400 - GLAS
      *
       01 KOMPL-S-SACH-GLAS.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-SG-ZWG                     PIC 9(002).
          05  KOMPL-S-SG-ZWG-TEXT                PIC X(003).
          05  KOMPL-S-SG-WOHNFLAECHE             PIC 9(005).
          05  KOMPL-S-SG-GEFAHR-INFO OCCURS  10.
              10  KOMPL-S-SG-GEFAHR              PIC 9(009).
              10  KOMPL-S-SG-BERECH-NACH         PIC 9(002).
              10  KOMPL-S-SG-BERECH-WERT         PIC 9(009).
          05  KOMPL-S-SG-ART-GEBAEUDE            PIC 9(001).
          05  KOMPL-S-SG-BEDING                  PIC 9(009) OCCURS 15.
          05  KOMPL-S-SG-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  KOMPL-S-SG-PRAEMIE                 PIC 9(005)V99.

      *
      *  SATZART 500 - HAFPFLICHT (ALLG. TEIL)
      *
       01 KOMPL-S-HAFTPFLICHT.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-H-ZWG                      PIC 9(002).
          05  KOMPL-S-H-ZWG-TEXT                 PIC X(003).
          05  KOMPL-S-H-BEDING                   PIC 9(009) OCCURS 15.
          05  KOMPL-S-H-KLAUSEL                  PIC 9(009) OCCURS 15.
          05  KOMPL-S-H-PRODUKT                  PIC 9(002).

      *
      *  SATZART 510 / 550  - HAFTPFLICHT (BES. TEIL GEWERBE/PRFIVT-HP)
      *
       01 KOMPL-S-H-PRODUKT-GRUND-ZUS.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-H-GRUND-ZUS-KZ             PIC X(001).
          05  KOMPL-S-H-WANR-MENGE OCCURS 20.
               10  KOMPL-S-H-WANR                PIC 9(006).
               10  KOMPL-S-H-MENGE               PIC 9(007).
               10  KOMPL-S-H-MENGE-TEXT          PIC X(006).
           05  KOMPL-S-H-VSUM-PERS               PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-SACH               PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-VERM               PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-EK                 PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-MIET               PIC 9(008)V99.
           05  KOMPL-S-H-PRAEMIE                 PIC 9(005)V99.
           05  KOMPL-S-H-VSUM-PERS-SB            PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-SACH-SB            PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-VERM-SB            PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-EK-SB              PIC 9(008)V99.
           05  KOMPL-S-H-VSUM-MIET-SB            PIC 9(008)V99.
           05  KOMPL-S-H-LEBENSPARTNER           PIC X(021).

      *
      *  SATZART 600 - UNFALL (ALLG. TEIL)
      *
       01 KOMPL-S-UNFALL.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-U-ZWG                      PIC 9(002).
          05  KOMPL-S-U-ZWG-TEXT                 PIC X(003).
          05  KOMPL-S-U-BEDING                   PIC 9(009) OCCURS 15.
          05  KOMPL-S-U-KLAUSEL                  PIC 9(009) OCCURS 15.

      *
      *  SATZART 610 - UNFALL (PERSONENBEZOGENER TEIL)
      *
       01 KOMPL-S-U-DTUTAB.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-U-PROGRESSION              PIC 9(004).
          05  KOMPL-S-U-DYNAMIKPROZ              PIC 9(002)V99.
          05  KOMPL-S-U-NAME                     PIC X(021).
          05  KOMPL-S-U-GEBDAT                   PIC X(008).
          05  FILLER REDEFINES KOMPL-S-U-GEBDAT.
              10  FILLER                         PIC 9(002).
              10  KOMPL-S-U-GEBDAT-JMT           PIC 9(006).
          05  FILLER REDEFINES KOMPL-S-U-GEBDAT.
              10  KOMPL-S-U-GEBDAT-JH            PIC 9(002).
              10  KOMPL-S-U-GEBDAT-JJ            PIC 9(002).
              10  FILLER                         PIC 9(004).
          05  KOMPL-S-U-BERUF                    PIC 9(003).
          05  KOMPL-S-U-BERUF-GEN                PIC 9(001).
          05  KOMPL-S-U-DECKUNG                  PIC 9(001).
          05  KOMPL-S-U-VSUM-TOD                 PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-INV                 PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-KTG                 PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-TG                  PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-GG                  PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-HEIL                PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-HHILF               PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-SCHULGE             PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-GRUNDSUM            PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-UEBERG              PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-KOSMETIK            PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-BERGUNG             PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-KUR                 PIC 9(008)V99.
          05  KOMPL-S-U-VSUM-RENTE               PIC 9(008)V99.
          05  KOMPL-S-U-PRAEMIE                  PIC 9(005)V99.

      *
      *  SATZART 700 - SONSTIGE SACH (GEWERBE)
      *
       01 KOMPL-S-SONSTIGE-SACH.
          05  FILLER                             PIC X(021).
          05  KOMPL-S-SONSTIGE-SACH-DATEN.
              10  KOMPL-S-SS-LFDNR               PIC 9(002).
              10  KOMPL-S-SS-ZWG                 PIC 9(002).
              10  KOMPL-S-SS-ZWG-TEXT            PIC X(003).
              10  KOMPL-S-SS-VSUMME              PIC 9(009)V99.
              10  KOMPL-S-SS-BEDOCC OCCURS 15.
                  15  KOMPL-S-SS-BEDING          PIC 9(009).
              10  KOMPL-S-SS-KLAOCC OCCURS 15.
                  15  KOMPL-S-SS-KLAUSEL         PIC 9(009).
              10  KOMPL-S-SS-PRAEMIE             PIC 9(005)V99.

      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI0955                       *
      ******************************************************************
