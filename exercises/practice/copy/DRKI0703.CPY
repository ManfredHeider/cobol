      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0703.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0703.cpv  $
      *
      *   Rev 2.1   Mar 03 1999 11:45:48   I13PC16
      *Konvertierung ASCII nach Ansi
      *
      *   Rev 2.0   Mar 01 1999 14:28:14   I13PC16
      *COPY-MEMBER FÜR EINGABEDATEI AMV-SAVARIDATEN MIT VORSATZ
      *
      *   Rev 1.0   06 Oct 1998 15:30:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Eingabedatei AMV-Savaridaten mit Vorsatz                *
      *                                                               *
      *    SATZLÄNGE   11711                                          *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *21.10.98!NEUERSTELLUNG                         !B.GOLÜCKE! 01  *
      *        !                                      !         !     *
      *****************************************************************

       01 DRKI0703.

         05 AMV-SACH-VORSATZ.
      *
            10 AMV-SACH-V-SA                      PIC X(001).
      *        SATZART = 'A'
            10 AMV-SACH-V-ABSENDER                PIC X(003).
      *        ABSENDER = 'AMV'
            10 AMV-SACH-V-DATUM                   PIC 9(008).
      *        ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
            10 AMV-SACH-V-EMPFAENGER              PIC X(004).
      *        EMPFAENGER = 'DVAG'
            10 AMV-SACH-V-TEXT                    PIC X(016).
      *        INHALTSANGABE = 'SAVARI-BESTAND'
            10 AMV-SACH-V-KZ                      PIC X(004).
      *        INHALTSANGABE = 'INIT' für Komplettbestand
      *                        'AEND' für Änderungsbestand
            10 FILLER                             PIC X(11675).
      *        RESERVE

         05 AMV-SACH-SATZ REDEFINES AMV-SACH-VORSATZ.
      *
      *  Allgemeine Daten
      *
            10 AMV-S-ALLG-DATEN.
               15  AMV-S-A-AEND-KZ                PIC 9(002).
               15  AMV-S-A-VSNR                   PIC 9(015).
               15  AMV-S-A-DVAG-VSNR              PIC 9(007).
               15  AMV-S-A-DVAG-PRUEF             PIC 9(002).
               15  AMV-S-A-BEGINN                 PIC 9(008).
               15  FILLER REDEFINES AMV-S-A-BEGINN.
                   20  AMV-S-A-BEG-JJJJ           PIC 9(004).
                   20  AMV-S-A-BEG-MM             PIC 9(002).
                   20  AMV-S-A-BEG-TT             PIC 9(002).
               15  FILLER REDEFINES AMV-S-A-BEGINN.
                   20  AMV-S-A-BEG-JH             PIC 9(002).
                   20  AMV-S-A-BEG-JJ             PIC 9(002).
                   20  FILLER                     PIC 9(004).
               15  AMV-S-A-ABLAUF                 PIC 9(008).
               15  FILLER REDEFINES AMV-S-A-ABLAUF.
                   20  AMV-S-A-ABL-JJJJ           PIC 9(004).
                   20  AMV-S-A-ABL-MM             PIC 9(002).
                   20  AMV-S-A-ABL-TT             PIC 9(002).
               15  FILLER REDEFINES AMV-S-A-ABLAUF.
                   20  AMV-S-A-ABL-JH             PIC 9(002).
                   20  AMV-S-A-ABL-JJ             PIC 9(002).
                   20  FILLER                     PIC 9(004).
               15  AMV-S-A-VERZUSTAND             PIC X(001).
               15  AMV-S-A-BLZ                    PIC 9(011).
               15  AMV-S-A-KTONR                  PIC 9(018).
               15  AMV-S-A-ZW                     PIC X(001).
               15  AMV-S-A-BEITRAG                PIC 9(008)V99.
               15  AMV-S-A-DVAG-VBNR              PIC 9(007).
               15  AMV-S-A-AENDBEG                PIC 9(008).
               15  FILLER REDEFINES AMV-S-A-AENDBEG.
                   20  AMV-S-A-AEND-JJJJ          PIC 9(004).
                   20  AMV-S-A-AEND-MM            PIC 9(002).
                   20  AMV-S-A-AEND-TT            PIC 9(002).
               15  FILLER REDEFINES AMV-S-A-AENDBEG.
                   20  AMV-S-A-AEND-JH            PIC 9(002).
                   20  AMV-S-A-AEND-JJ            PIC 9(002).
                   20  FILLER                     PIC 9(004).
               15  AMV-S-A-LBDAT                  PIC 9(008).
               15  FILLER REDEFINES AMV-S-A-LBDAT.
                   20  AMV-S-A-LBDAT-JJJJ         PIC 9(004).
                   20  AMV-S-A-LBDAT-MM           PIC 9(002).
                   20  AMV-S-A-LBDAT-TT           PIC 9(002).
               15  FILLER REDEFINES AMV-S-A-LBDAT.
                   20  AMV-S-A-LBDAT-JH           PIC 9(002).
                   20  AMV-S-A-LBDAT-JJ           PIC 9(002).
                   20  FILLER                     PIC 9(004).
               15  AMV-S-A-LBUHRZEIT              PIC 9(008).
               15  AMV-S-A-INKASSO                PIC X(001).
               15  AMV-S-A-STORNO                 PIC 9(008).
               15  FILLER REDEFINES AMV-S-A-STORNO.
                   20  AMV-S-A-STORNO-JJJJ        PIC 9(004).
                   20  AMV-S-A-STORNO-MM          PIC 9(002).
                   20  AMV-S-A-STORNO-TT          PIC 9(002).
               15  FILLER REDEFINES AMV-S-A-STORNO.
                   20  AMV-S-A-STORNO-JH          PIC 9(002).
                   20  AMV-S-A-STORNO-JJ          PIC 9(002).
                   20  FILLER                     PIC 9(004).
               15  AMV-S-A-NACHL-TABELLE.
                   20 AMV-S-A-NACHL-TABELLE-SAV OCCURS 10.
                      25 AMV-S-A-NACHL-TEXT       PIC X(026).
                      25 AMV-S-A-NACHL-EINHEIT    PIC 9(001).
                      25 AMV-S-A-NACHL-WERT       PIC 9(008)V99.
               15  AMV-S-A-GEB-VERTRAG            PIC X(001).
               15  AMV-S-A-VORT-LKZ               PIC X(003).
               15  AMV-S-A-VORT-PLZ               PIC X(005).
               15  AMV-S-A-VORT-STR               PIC X(021).
               15  AMV-S-A-VORT-ORT               PIC X(021).
               15  AMV-S-A-WAEHRUNG-KZ            PIC X(003).
               15  AMV-S-A-FILLER                 PIC X(320).
      *
      *  Daten aus SAVARI
      *
            10 AMV-S-SACH-DATEN.
               15 AMV-S-SACH-WOHNGEB.
                  20  AMV-S-SW-ZWG                PIC 9(002).
                  20  AMV-S-SW-ZWG-TEXT           PIC X(003).
                  20  AMV-S-SW-GEFAHR-ZONE-SB OCCURS 5.
                      25  AMV-S-SW-GEFAHR         PIC X(003).
                      25  AMV-S-SW-ZONE           PIC 9(001).
                  20  AMV-S-SW-ST-SB.
                      25  AMV-S-SW-ST-VSUMME      PIC 9(002).
                      25  AMV-S-SW-ST-SCHADEN     PIC 9(002).
                      25  AMV-S-SW-ST-MIN         PIC 9(005).
                      25  AMV-S-SW-ST-MAX         PIC 9(005).
                  20  AMV-S-SW-LW-SB.
                      25  AMV-S-SW-LW-VSUMME      PIC 9(002).
                      25  AMV-S-SW-LW-SCHADEN     PIC 9(002).
                      25  AMV-S-SW-LW-MIN         PIC 9(005).
                      25  AMV-S-SW-LW-MAX         PIC 9(005).
                  20  AMV-S-SW-EL-ERD-SB.
                      25  AMV-S-SW-EL-ERD-VSUMME  PIC 9(002).
                      25  AMV-S-SW-EL-ERD-SCHADEN PIC 9(002).
                      25  AMV-S-SW-EL-ERD-MIN     PIC 9(005).
                      25  AMV-S-SW-EL-ERD-MAX     PIC 9(005).
                  20  AMV-S-SW-EL-SON-SB.
                      25  AMV-S-SW-EL-SON-VSUMME  PIC 9(002).
                      25  AMV-S-SW-EL-SON-SCHADEN PIC 9(002).
                      25  AMV-S-SW-EL-SON-MIN     PIC 9(005).
                      25  AMV-S-SW-EL-SON-MAX     PIC 9(005).
                  20  AMV-S-SW-VSUMME             PIC 9(009)V99.
                  20  AMV-S-SW-SB-KZ              PIC X(002).
                  20  AMV-S-SW-LEISTUNG           PIC 9(002).
                  20  AMV-S-SW-BAUART-KLASSE      PIC X(002).
                  20  AMV-S-SW-BAUJAHR            PIC 9(004).
                  20  AMV-S-SW-ART-GEBAEUDE       PIC 9(002).
                  20  AMV-S-SW-GN-FAKTOR          PIC 9(002)V9.
                  20  AMV-S-SW-VGB88-GARAGE       PIC X(001).
                  20  AMV-S-SW-VGB88-NEBENGEB     PIC X(001).
                  20  AMV-S-SW-VGB88-ANBAU        PIC X(001).
                  20  AMV-S-SW-VGB88-CARPORT      PIC X(001).
                  20  AMV-S-SW-UMFANG             PIC 9(003) OCCURS 20.
                  20  AMV-S-SW-BEDING             PIC 9(002) OCCURS 15.
                  20  AMV-S-SW-KLAUSEL            PIC 9(004) OCCURS 15.
                  20  AMV-S-SW-NETTO-PRAEM        PIC 9(008)V99.
                  20  AMV-S-SW-FILLER             PIC X(100).

               15 AMV-S-SACH-HAUSRAT.
                  20  AMV-S-SH-ZWG                PIC 9(002).
                  20  AMV-S-SH-ZWG-TEXT           PIC X(003).
                  20  AMV-S-SH-GEFAHR             PIC X(003) OCCURS 6.
                  20  AMV-S-SH-VSUMME             PIC 9(009)V99.
                  20  AMV-S-SH-SB-KZ              PIC X(002).
                  20  AMV-S-SH-LEISTUNG           PIC 9(002).
                  20  AMV-S-SH-ZONE               PIC 9(001).
                  20  AMV-S-SH-ART-GEBAEUDE       PIC 9(001).
                  20  AMV-S-SH-BEWOHNT            PIC X(001).
                  20  AMV-S-SH-QM-WOHNFL          PIC 9(005).
                  20  AMV-S-SH-UMFANG-PROZ OCCURS  5.
                      25  AMV-S-SH-UMFANG         PIC 9(003).
                      25  AMV-S-SH-PROZ           PIC 9(002)V9.
                  20  AMV-S-SH-BEDING             PIC 9(002) OCCURS 15.
                  20  AMV-S-SH-KLAUSEL            PIC 9(004) OCCURS 15.
                  20  AMV-S-SH-PRAEMIE            PIC 9(005)V99.
                  20  AMV-S-SH-FILLER             PIC X(100).

               15 AMV-S-SACH-GLAS.
                  20  AMV-S-SG-ZWG                PIC 9(002).
                  20  AMV-S-SG-ZWG-TEXT           PIC X(003).
                  20  AMV-S-SG-WOHNFLAECHE        PIC 9(005).
                  20  AMV-S-SG-GEFAHR-INFO OCCURS  10.
                      25  AMV-S-SG-GEFAHR         PIC 9(003).
                      25  AMV-S-SG-BERECH-NACH    PIC 9(002).
                      25  AMV-S-SG-BERECH-WERT    PIC 9(009).
                  20  AMV-S-SG-ART-GEBAEUDE       PIC 9(001).
                  20  AMV-S-SG-BEDING             PIC 9(002) OCCURS 15.
                  20  AMV-S-SG-KLAUSEL            PIC 9(004) OCCURS 15.
                  20  AMV-S-SG-PRAEMIE            PIC 9(005)V99.
                  20  AMV-S-SG-FILLER             PIC X(100).

               15 AMV-S-HAFTPFLICHT.
                  20  AMV-S-H-ZWG                 PIC 9(002).
                  20  AMV-S-H-ZWG-TEXT            PIC X(003).
                  20  AMV-S-H-BEDING              PIC 9(002) OCCURS 15.
                  20  AMV-S-H-KLAUSEL             PIC 9(004) OCCURS 15.
                  20  AMV-S-H-PRODUKT             PIC 9(002).
                  20  AMV-S-H-PRODUKT-GRUND-ZUS OCCURS 2.
                      25  AMV-S-H-GRUND-ZUS-KZ    PIC X(001).
                      25  AMV-S-H-WANR-MENGE OCCURS 20.
                          30  AMV-S-H-WANR        PIC 9(004).
                          30  AMV-S-H-MENGE       PIC 9(007).
                          30  AMV-S-H-MENGE-TEXT  PIC X(006).
                      25  AMV-S-H-VSUM-PERS       PIC 9(008)V99.
                      25  AMV-S-H-VSUM-SACH       PIC 9(008)V99.
                      25  AMV-S-H-VSUM-VERM       PIC 9(008)V99.
                      25  AMV-S-H-VSUM-EK         PIC 9(008)V99.
                      25  AMV-S-H-VSUM-MIET       PIC 9(008)V99.
                      25  AMV-S-H-PRAEMIE         PIC 9(005)V99.
                      25  AMV-S-H-VSUM-PERS-SB    PIC 9(008)V99.
                      25  AMV-S-H-VSUM-SACH-SB    PIC 9(008)V99.
                      25  AMV-S-H-VSUM-VERM-SB    PIC 9(008)V99.
                      25  AMV-S-H-VSUM-EK-SB      PIC 9(008)V99.
                      25  AMV-S-H-VSUM-MIET-SB    PIC 9(008)V99.
                      25  AMV-S-H-LEBENSPARTNER   PIC X(021).
                  20  AMV-S-H-FILLER              PIC X(90).

               15 AMV-S-UNFALL.
                  20  AMV-S-U-ZWG                 PIC 9(002).
                  20  AMV-S-U-ZWG-TEXT            PIC X(003).
                  20  AMV-S-U-BEDING              PIC 9(002) OCCURS 15.
                  20  AMV-S-U-KLAUSEL             PIC 9(004) OCCURS 15.
                  20  AMV-S-U-DTUTAB OCCURS 33.
                      25  AMV-S-U-PROGRESSION     PIC 9(004).
                      25  AMV-S-U-DYNAMIKPROZ     PIC 9(002)V99.
                      25  AMV-S-U-NAME            PIC X(021).
                      25  AMV-S-U-GEBDAT          PIC X(008).
                      25  FILLER REDEFINES AMV-S-U-GEBDAT.
                          30  FILLER              PIC 9(002).
                          30  AMV-S-U-GEBDAT-JMT  PIC 9(006).
                      25  FILLER REDEFINES AMV-S-U-GEBDAT.
                          30  AMV-S-U-GEBDAT-JH   PIC 9(002).
                          30  AMV-S-U-GEBDAT-JJ   PIC 9(002).
                          30  FILLER              PIC 9(004).
                      25  AMV-S-U-BERUF           PIC 9(003).
                      25  AMV-S-U-BERUF-GEN       PIC 9(001).
                      25  AMV-S-U-DECKUNG         PIC 9(001).
                      25  AMV-S-U-VSUM-TOD        PIC 9(008)V99.
                      25  AMV-S-U-VSUM-INV        PIC 9(008)V99.
                      25  AMV-S-U-VSUM-KTG        PIC 9(008)V99.
                      25  AMV-S-U-VSUM-TG         PIC 9(008)V99.
                      25  AMV-S-U-VSUM-GG         PIC 9(008)V99.
                      25  AMV-S-U-VSUM-HEIL       PIC 9(008)V99.
                      25  AMV-S-U-VSUM-HHILF      PIC 9(008)V99.
                      25  AMV-S-U-VSUM-SCHULGE    PIC 9(008)V99.
                      25  AMV-S-U-VSUM-GRUNDSUM   PIC 9(008)V99.
                      25  AMV-S-U-VSUM-UEBERG     PIC 9(008)V99.
                      25  AMV-S-U-VSUM-KOSMETIK   PIC 9(008)V99.
                      25  AMV-S-U-VSUM-BERGUNG    PIC 9(008)V99.
                      25  AMV-S-U-VSUM-KUR        PIC 9(008)V99.
                      25  AMV-S-U-VSUM-RENTE      PIC 9(008)V99.
                      25  AMV-S-U-PRAEMIE         PIC 9(005)V99.
                  20  AMV-S-U-FILLER              PIC X(100).

               15 AMV-S-SONSTIGE-SACH.
                  20  AMV-S-SONSTIGE-SACH-DATEN OCCURS 9.
                      25  AMV-S-SS-LFDNR          PIC 9(002).
                      25  AMV-S-SS-ZWG            PIC 9(002).
                      25  AMV-S-SS-ZWG-TEXT       PIC X(003).
                      25  AMV-S-SS-VSUMME         PIC 9(009)V99.
                      25  AMV-S-SS-BEDOCC OCCURS 15.
                          30  AMV-S-SS-BEDING     PIC 9(002).
                      25  AMV-S-SS-KLAOCC OCCURS 15.
                          30  AMV-S-SS-KLAUSEL    PIC 9(004).
                      25  AMV-S-SS-PRAEMIE        PIC 9(005)V99.
                      25  AMV-S-SS-FILLER         PIC X(100).
               15 FILLER                          PIC X(360).

           05  AMV-SACH-NACHSATZ REDEFINES AMV-SACH-VORSATZ.
      *
               10  AMV-SACH-N-SA                  PIC X(001).
      *            SATZART = 'E'
               10  AMV-SACH-N-SATZANZAHL          PIC 9(009).
      *            SATZANZAHL OHNE VOR- UND NACHSATZ
               10  FILLER                         PIC X(11701).
      *            RESERVE

      ******************************************************************
      *          E n d e    Copy-Member DRKI0703                       *
      ******************************************************************
