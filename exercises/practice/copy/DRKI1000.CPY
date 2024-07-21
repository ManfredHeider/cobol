      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1000.cpy  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:\MF\CPY\drki1000.cpv  $
      *
      *   Rev 2.6   28 Sep 2000 10:52:18   T13EX41
      *AKI00064
      *- Umstellung SAVARI auf Satzarten
      *
      *   Rev 2.5   09 May 2000 15:20:12   T13EX27
      *AMV Neue Satzstrukturen
      *
      *   Rev 2.4   Apr 26 2000 15:50:18   T13EX27
      *AKI000064
      *Umsetzen alte/neue Satzarten
      *
      *   Rev 2.3   Apr 25 2000 16:04:06   T13EX14
      *AKI000064
      *Umsetzen alte in neue Satzarten
      *Struktur der neuen Satzarten
      *
      *   Rev 2.2   Apr 25 2000 15:55:40   T13EX14
      *AKI000064
      *Umsetzen alte in neue Satzarten
      *Struktur der neuen Satzarten
      *
      *   Rev 2.1   Apr 25 2000 15:11:28   T13EX14
      *AKI000064
      *Umsetzen alte in neue Satzarten
      *Struktur der neuen Satzarten
      *
      *   Rev 2.0   Feb 09 2000 14:16:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - AUSGABEDATEI  SAVARI-VERTRAEGE  NEUE SATZARTEN          *
      *                                                               *
      *    SATZLÄNGE   VAR     214 min    596 max                     *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *21.02.00! NEU                                  ! LENNARTZ! 2.0 *
      *AKI00064!                                      !         !     *
      *****************************************************************
      *
      *
      *  SATZART 100 - ALLGEMEINE DATEN
      *
       01 VAR-S-ALLG-DATEN.
          05  VAR-S-A-KEY.
              10  VAR-S-A-VSNR-KEY             PIC 9(015).
              10  VAR-S-A-SATZART-KEY          PIC 9(003).
              10  VAR-S-A-LFDNR-KEY            PIC 9(003).
          05  VAR-S-A-AEND-KZ                  PIC 9(002).
          05  VAR-S-A-VSNR                     PIC 9(015).
          05  VAR-S-A-DVAG-VSNR                PIC 9(007).
          05  VAR-S-A-DVAG-PRUEF               PIC 9(002).
          05  VAR-S-A-BEGINN                   PIC 9(008).
          05  FILLER REDEFINES VAR-S-A-BEGINN.
              10  VAR-S-A-BEG-JJJJ             PIC 9(004).
              10  VAR-S-A-BEG-MM               PIC 9(002).
              10  VAR-S-A-BEG-TT               PIC 9(002).
          05  FILLER REDEFINES VAR-S-A-BEGINN.
              10  VAR-S-A-BEG-JH               PIC 9(002).
              10  VAR-S-A-BEG-JJ               PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-A-ABLAUF                   PIC 9(008).
          05  FILLER REDEFINES VAR-S-A-ABLAUF.
              10  VAR-S-A-ABL-JJJJ             PIC 9(004).
              10  VAR-S-A-ABL-MM               PIC 9(002).
              10  VAR-S-A-ABL-TT               PIC 9(002).
          05  FILLER REDEFINES VAR-S-A-ABLAUF.
              10  VAR-S-A-ABL-JH               PIC 9(002).
              10  VAR-S-A-ABL-JJ               PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-A-VERZUSTAND               PIC X(001).
          05  VAR-S-A-BLZ                      PIC 9(011).
          05  VAR-S-A-KTONR                    PIC 9(018).
          05  VAR-S-A-ZW                       PIC X(001).
          05  VAR-S-A-BEITRAG                  PIC 9(008)V99.
          05  VAR-S-A-DVAG-VBNR                PIC 9(007).
          05  VAR-S-A-AENDBEG                  PIC 9(008).
          05  FILLER REDEFINES VAR-S-A-AENDBEG.
              10  VAR-S-A-AEND-JJJJ            PIC 9(004).
              10  VAR-S-A-AEND-MM              PIC 9(002).
              10  VAR-S-A-AEND-TT              PIC 9(002).
          05  FILLER REDEFINES VAR-S-A-AENDBEG.
              10  VAR-S-A-AEND-JH              PIC 9(002).
              10  VAR-S-A-AEND-JJ              PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-A-LBDAT                    PIC 9(008).
          05  FILLER REDEFINES VAR-S-A-LBDAT.
              10  VAR-S-A-LBDAT-JJJJ           PIC 9(004).
              10  VAR-S-A-LBDAT-MM             PIC 9(002).
              10  VAR-S-A-LBDAT-TT             PIC 9(002).
          05  FILLER REDEFINES VAR-S-A-LBDAT.
              10  VAR-S-A-LBDAT-JH             PIC 9(002).
              10  VAR-S-A-LBDAT-JJ             PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-A-LBUHRZEIT                PIC 9(008).
          05  VAR-S-A-INKASSO                  PIC X(001).
          05  VAR-S-A-STORNO                   PIC 9(008).
          05  FILLER REDEFINES VAR-S-A-STORNO.
              10  VAR-S-A-STORNO-JJJJ          PIC 9(004).
              10  VAR-S-A-STORNO-MM            PIC 9(002).
              10  VAR-S-A-STORNO-TT            PIC 9(002).
          05  FILLER REDEFINES VAR-S-A-STORNO.
              10  VAR-S-A-STORNO-JH            PIC 9(002).
              10  VAR-S-A-STORNO-JJ            PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-A-NACHL-TABELLE.
              10 VAR-S-A-NACHL-TABELLE-SAV OCCURS 10.
                 15 VAR-S-A-NACHL-TEXT         PIC X(026).
                 15 VAR-S-A-NACHL-EINHEIT      PIC 9(001).
                 15 VAR-S-A-NACHL-WERT         PIC 9(008)V99.
          05  VAR-S-A-GEB-VERTRAG              PIC X(001).
          05  VAR-S-A-VORT-LKZ                 PIC X(003).
          05  VAR-S-A-VORT-PLZ                 PIC X(005).
          05  VAR-S-A-VORT-STR                 PIC X(021).
          05  VAR-S-A-VORT-ORT                 PIC X(021).
          05  VAR-S-A-WAEHRUNG-KZ              PIC X(003).
      *
      *  SATZART 200 - WOHNGEBAEUDE
      *
       01 VAR-S-SACH-WOHNGEB.
          05  VAR-S-SW-KEY.
              10  VAR-S-SW-VSNR-KEY            PIC 9(015).
              10  VAR-S-SW-SATZART-KEY         PIC 9(003).
              10  VAR-S-SW-LFDNR-KEY           PIC 9(003).
          05  VAR-S-SW-ZWG                     PIC 9(002).
          05  VAR-S-SW-ZWG-TEXT                PIC X(003).
          05  VAR-S-SW-GEFAHR-ZONE-SB OCCURS 5.
              10  VAR-S-SW-GEFAHR              PIC X(003).
              10  VAR-S-SW-ZONE                PIC 9(001).
          05  VAR-S-SW-ST-SB.
              10  VAR-S-SW-ST-VSUMME           PIC 9(002).
              10  VAR-S-SW-ST-SCHADEN          PIC 9(002).
              10  VAR-S-SW-ST-MIN              PIC 9(005).
              10  VAR-S-SW-ST-MAX              PIC 9(005).
          05  VAR-S-SW-LW-SB.
              10  VAR-S-SW-LW-VSUMME           PIC 9(002).
              10  VAR-S-SW-LW-SCHADEN          PIC 9(002).
              10  VAR-S-SW-LW-MIN              PIC 9(005).
              10  VAR-S-SW-LW-MAX              PIC 9(005).
          05  VAR-S-SW-EL-ERD-SB.
              10  VAR-S-SW-EL-ERD-VSUMME       PIC 9(002).
              10  VAR-S-SW-EL-ERD-SCHADEN      PIC 9(002).
              10  VAR-S-SW-EL-ERD-MIN          PIC 9(005).
              10  VAR-S-SW-EL-ERD-MAX          PIC 9(005).
          05  VAR-S-SW-EL-SON-SB.
              10  VAR-S-SW-EL-SON-VSUMME       PIC 9(002).
              10  VAR-S-SW-EL-SON-SCHADEN      PIC 9(002).
              10  VAR-S-SW-EL-SON-MIN          PIC 9(005).
              10  VAR-S-SW-EL-SON-MAX          PIC 9(005).
          05  VAR-S-SW-VSUMME                  PIC 9(009)V99.
          05  VAR-S-SW-SB-KZ                   PIC X(002).
          05  VAR-S-SW-LEISTUNG                PIC 9(002).
          05  VAR-S-SW-BAUART-KLASSE           PIC X(002).
          05  VAR-S-SW-BAUJAHR                 PIC 9(004).
          05  VAR-S-SW-ART-GEBAEUDE            PIC 9(002).
          05  VAR-S-SW-GN-FAKTOR               PIC 9(002)V9.
          05  VAR-S-SW-VGB88-GARAGE            PIC X(001).
          05  VAR-S-SW-VGB88-NEBENGEB          PIC X(001).
          05  VAR-S-SW-VGB88-ANBAU             PIC X(001).
          05  VAR-S-SW-VGB88-CARPORT           PIC X(001).
          05  VAR-S-SW-UMFANG                  PIC 9(009) OCCURS 20.
          05  VAR-S-SW-BEDING                  PIC 9(009) OCCURS 15.
          05  VAR-S-SW-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  VAR-S-SW-NETTO-PRAEM             PIC 9(008)V99.

      *
      *  SATZART 300 - HAUSRAT
      *
       01 VAR-S-SACH-HAUSRAT.
          05  VAR-S-SH-KEY.
              10  VAR-S-SH-VSNR-KEY            PIC 9(015).
              10  VAR-S-SH-SATZART-KEY         PIC 9(003).
              10  VAR-S-SH-LFDNR-KEY           PIC 9(003).
          05  VAR-S-SH-ZWG                     PIC 9(002).
          05  VAR-S-SH-ZWG-TEXT                PIC X(003).
          05  VAR-S-SH-GEFAHR                  PIC X(003) OCCURS 6.
          05  VAR-S-SH-VSUMME                  PIC 9(009)V99.
          05  VAR-S-SH-SB-KZ                   PIC X(002).
          05  VAR-S-SH-LEISTUNG                PIC 9(002).
          05  VAR-S-SH-ZONE                    PIC 9(001).
          05  VAR-S-SH-ART-GEBAEUDE            PIC 9(001).
          05  VAR-S-SH-BEWOHNT                 PIC X(001).
          05  VAR-S-SH-QM-WOHNFL               PIC 9(005).
          05  VAR-S-SH-UMFANG-PROZ OCCURS  20.
              10  VAR-S-SH-UMFANG              PIC 9(009).
              10  VAR-S-SH-PROZ                PIC 9(002)V9.
          05  VAR-S-SH-BEDING                  PIC 9(009) OCCURS 15.
          05  VAR-S-SH-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  VAR-S-SH-PRAEMIE                 PIC 9(005)V99.

      *
      *  SATZART 400 - GLAS
      *
       01 VAR-S-SACH-GLAS.
          05  VAR-S-SG-KEY.
              10  VAR-S-SG-VSNR-KEY            PIC 9(015).
              10  VAR-S-SG-SATZART-KEY         PIC 9(003).
              10  VAR-S-SG-LFDNR-KEY           PIC 9(003).
          05  VAR-S-SG-ZWG                     PIC 9(002).
          05  VAR-S-SG-ZWG-TEXT                PIC X(003).
          05  VAR-S-SG-WOHNFLAECHE             PIC 9(005).
          05  VAR-S-SG-GEFAHR-INFO OCCURS  10.
              10  VAR-S-SG-GEFAHR              PIC 9(009).
              10  VAR-S-SG-BERECH-NACH         PIC 9(002).
              10  VAR-S-SG-BERECH-WERT         PIC 9(009).
          05  VAR-S-SG-ART-GEBAEUDE            PIC 9(001).
          05  VAR-S-SG-BEDING                  PIC 9(009) OCCURS 15.
          05  VAR-S-SG-KLAUSEL                 PIC 9(009) OCCURS 15.
          05  VAR-S-SG-PRAEMIE                 PIC 9(005)V99.

      *
      *  SATZART 500 - HAFPFLICHT (ALLG. TEIL)
      *
       01 VAR-S-HAFTPFLICHT.
          05  VAR-S-H-KEY.
              10  VAR-S-H-VSNR-KEY             PIC 9(015).
              10  VAR-S-H-SATZART-KEY          PIC 9(003).
              10  VAR-S-H-LFDNR-KEY            PIC 9(003).
          05  VAR-S-H-ZWG                      PIC 9(002).
          05  VAR-S-H-ZWG-TEXT                 PIC X(003).
          05  VAR-S-H-BEDING                   PIC 9(009) OCCURS 15.
          05  VAR-S-H-KLAUSEL                  PIC 9(009) OCCURS 15.
          05  VAR-S-H-PRODUKT                  PIC 9(002).

      *
      *  SATZART 510 / 550  - HAFTPFLICHT (BES. TEIL GEWERBE/PRFIVT-HP)
      *
       01 VAR-S-H-PRODUKT-GRUND-ZUS.
          05  VAR-S-HF-KEY.
              10  VAR-S-HF-VSNR-KEY            PIC 9(015).
              10  VAR-S-HF-SATZART-KEY         PIC 9(003).
              10  VAR-S-HF-LFDNR-KEY           PIC 9(003).
          05  VAR-S-H-GRUND-ZUS-KZ             PIC X(001).
          05  VAR-S-H-WANR-MENGE OCCURS 20.
               10  VAR-S-H-WANR                PIC 9(006).
               10  VAR-S-H-MENGE               PIC 9(007).
               10  VAR-S-H-MENGE-TEXT          PIC X(006).
           05  VAR-S-H-VSUM-PERS               PIC 9(008)V99.
           05  VAR-S-H-VSUM-SACH               PIC 9(008)V99.
           05  VAR-S-H-VSUM-VERM               PIC 9(008)V99.
           05  VAR-S-H-VSUM-EK                 PIC 9(008)V99.
           05  VAR-S-H-VSUM-MIET               PIC 9(008)V99.
           05  VAR-S-H-PRAEMIE                 PIC 9(005)V99.
           05  VAR-S-H-VSUM-PERS-SB            PIC 9(008)V99.
           05  VAR-S-H-VSUM-SACH-SB            PIC 9(008)V99.
           05  VAR-S-H-VSUM-VERM-SB            PIC 9(008)V99.
           05  VAR-S-H-VSUM-EK-SB              PIC 9(008)V99.
           05  VAR-S-H-VSUM-MIET-SB            PIC 9(008)V99.
           05  VAR-S-H-LEBENSPARTNER           PIC X(021).

      *
      *  SATZART 600 - UNFALL (ALLG. TEIL)
      *
       01 VAR-S-UNFALL.
          05  VAR-S-U-KEY.
              10  VAR-S-U-VSNR-KEY             PIC 9(015).
              10  VAR-S-U-SATZART-KEY          PIC 9(003).
              10  VAR-S-U-LFDNR-KEY            PIC 9(003).
          05  VAR-S-U-ZWG                      PIC 9(002).
          05  VAR-S-U-ZWG-TEXT                 PIC X(003).
          05  VAR-S-U-BEDING                   PIC 9(009) OCCURS 15.
          05  VAR-S-U-KLAUSEL                  PIC 9(009) OCCURS 15.

      *
      *  SATZART 610 - UNFALL (PERSONENBEZOGENER TEIL)
      *
       01 VAR-S-U-DTUTAB.
          05  VAR-S-SU-KEY.
              10  VAR-S-SU-VSNR-KEY            PIC 9(015).
              10  VAR-S-SU-SATZART-KEY         PIC 9(003).
              10  VAR-S-SU-LFDNR-KEY           PIC 9(003).
          05  VAR-S-U-PROGRESSION              PIC 9(004).
          05  VAR-S-U-DYNAMIKPROZ              PIC 9(002)V99.
          05  VAR-S-U-NAME                     PIC X(021).
          05  VAR-S-U-GEBDAT                   PIC X(008).
          05  FILLER REDEFINES VAR-S-U-GEBDAT.
              10  FILLER                       PIC 9(002).
              10  VAR-S-U-GEBDAT-JMT           PIC 9(006).
          05  FILLER REDEFINES VAR-S-U-GEBDAT.
              10  VAR-S-U-GEBDAT-JH            PIC 9(002).
              10  VAR-S-U-GEBDAT-JJ            PIC 9(002).
              10  FILLER                       PIC 9(004).
          05  VAR-S-U-BERUF                    PIC 9(003).
          05  VAR-S-U-BERUF-GEN                PIC 9(001).
          05  VAR-S-U-DECKUNG                  PIC 9(001).
          05  VAR-S-U-VSUM-TOD                 PIC 9(008)V99.
          05  VAR-S-U-VSUM-INV                 PIC 9(008)V99.
          05  VAR-S-U-VSUM-KTG                 PIC 9(008)V99.
          05  VAR-S-U-VSUM-TG                  PIC 9(008)V99.
          05  VAR-S-U-VSUM-GG                  PIC 9(008)V99.
          05  VAR-S-U-VSUM-HEIL                PIC 9(008)V99.
          05  VAR-S-U-VSUM-HHILF               PIC 9(008)V99.
          05  VAR-S-U-VSUM-SCHULGE             PIC 9(008)V99.
          05  VAR-S-U-VSUM-GRUNDSUM            PIC 9(008)V99.
          05  VAR-S-U-VSUM-UEBERG              PIC 9(008)V99.
          05  VAR-S-U-VSUM-KOSMETIK            PIC 9(008)V99.
          05  VAR-S-U-VSUM-BERGUNG             PIC 9(008)V99.
          05  VAR-S-U-VSUM-KUR                 PIC 9(008)V99.
          05  VAR-S-U-VSUM-RENTE               PIC 9(008)V99.
          05  VAR-S-U-PRAEMIE                  PIC 9(005)V99.

      *
      *  SATZART 700 - SONSTIGE SACH (GEWERBE)
      *
       01 VAR-S-SONSTIGE-SACH.
          05  VAR-S-SS-KEY.
              10  VAR-S-SS-VSNR-KEY            PIC 9(015).
              10  VAR-S-SS-SATZART-KEY         PIC 9(003).
              10  VAR-S-SS-LFDNR-KEY           PIC 9(003).
          05  VAR-S-SONSTIGE-SACH-DATEN.
              10  VAR-S-SS-LFDNR               PIC 9(002).
              10  VAR-S-SS-ZWG                 PIC 9(002).
              10  VAR-S-SS-ZWG-TEXT            PIC X(003).
              10  VAR-S-SS-VSUMME              PIC 9(009)V99.
              10  VAR-S-SS-BEDOCC OCCURS 15.
                  15  VAR-S-SS-BEDING          PIC 9(009).
              10  VAR-S-SS-KLAOCC OCCURS 15.
                  15  VAR-S-SS-KLAUSEL         PIC 9(009).
              10  VAR-S-SS-PRAEMIE             PIC 9(005)V99.

      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI1000                       *
      ******************************************************************

