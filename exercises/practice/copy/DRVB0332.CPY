      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0332.CPY  $
      *    $Revision:   2.25  $
      *
      *    $Log:   L:/mf/CPY/DRVB0332.CPv  $
      *
      *   Rev 2.25   May 22 2020 08:49:14   t13ae53
      *AVB01625: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen
      *
      *   Rev 2.24   May 11 2016 13:17:20   T13ae30
      *AVB01400: Projekt Lucy2 - Anpassung Stammdaten-DTA
      *
      *   Rev 2.23   Mar 22 2013 15:37:02   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.22   Mar 22 2013 15:02:54   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.21   Feb 24 2012 14:33:06   t13ae30
      *AVB01215: Erw. Stammdaten-DTA wg. BonusEH
      *
      *   Rev 2.20   Dec 15 2011 13:20:50   t13ae30
      *AAS00987: Änderung Erfolgsprovision für Central-Wechsler
      *
      *   Rev 2.19   Mar 10 2011 10:42:38   t13ae30
      *AAS00947: AVERM im Stammdaten DTA nach AC
      *
      *   Rev 2.18   May 27 2008 18:03:22   t13ae30
      *AVB00961: FVD Stufe 1 im VB-Verwaltungssystem TWI -2-
      *
      *   Rev 2.17   Apr 25 2008 10:47:06   t13ae30
      *AVB00970: Beschränkung Erfolgsprovision
      *
      *   Rev 2.16   31 Oct 2007 13:40:00   t13ae30
      *AVB00905: Allfinanz - Bausteine, Stammdaten-DTA TWI
      *
      *   Rev 2.15   Mar 08 2005 11:36:58   t13ae30
      *AAS00461: Erw. Stammdaten-DTA nach AC
      *
      *   Rev 2.14   Apr 16 2003 15:19:42   t13ae30
      *AVB00666 - VBAS UND ALPN in Batchprogrammen
      *
      *   Rev 2.13   28 Dec 2000 12:55:16   T13AE30
      *AVB00517 - Anpassung wegen Übernahme Abrechnungsdruck durch DVA-A
      *
      *   Rev 2.12   11 Aug 2000 11:42:38   T13AE30
      *AVB00500 - Einbau Ermittlung Praxisstufe
      *
      *   Rev 2.11   Jan 11 2000 16:23:28   T13AE30
      *AAS00051 - Stammdaten-DTA -> Aachen
      *
      *   Rev 2.10   Aug 10 1999 17:34:44   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.9   Aug 10 1999 16:34:02   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.8   Aug 10 1999 16:23:08   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.7   Aug 10 1999 14:43:24   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.6   Aug 10 1999 13:57:36   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.5   Aug 10 1999 09:53:56   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.4   Jul 22 1999 11:16:42   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.3   Jun 25 1999 08:20:40   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.2   Jun 25 1999 08:11:30   T13AE30
      *
      *
      *   Rev 2.1   Jun 02 1999 14:25:18   T13AE30
      *
      *
      *   Rev 2.0   Apr 13 1999 11:40:48   T13AE30
      *
      *
      *   Rev 1.0   27 Oct 1998 16:34:42   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0332                                      *
      *    --------------------------------------------------------   *
      *    <<DTA-SATZ WG. STAMMDATEN DTA -> AACHEN>>                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    29.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!29.10.98! 01 ! NEU                       ! WILLIG   *
      * AVB00500!02.08.00! 02 ! EINBAU PRAXISSTUFE        ! WILLIG   *
      * AVB00517!18.12.00! 05 ! VORNAME UND NAME AUF DB2- ! WILLIG   *
      *         !        !    ! FORMAT VERLÄNGERN (20, 40)!          *
      * AVB00666!11.04.03! 06 ! VBAS UND ALP IN BATCHPROG.! WILLIG   *
      * AAS00461!12.03.04! 09 ! UMFANGREICHE ERWEITERUNGEN! WILLIG   *
      *         !        !    ! UND ANPASSUNGEN           !          *
      * AVB00905!18.06.07! 10 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      * AVB00970!17.04.08! 11 ! NEUES KZ WG. ERFOLGSPROV. ! WILLIG   *
      * AVB00961!27.05.08! 12 ! EINFÜHRUNG FVD            ! WILLIG   *
      * AAS00947!28.01.11! 20 ! GROß-klein SCHREIBUNG     ! WILLIG   *
      * AAS00987!06.12.11! 21 ! CKV-WECHSLER              ! WILLIG   *
      * AVB01215!16.02.12! 22 ! BONUS-EH SCHWEIZER PARTNER! WILLIG   *
      * AVB01244!04.03.13! 23 ! ERMITTELN IBAN u. BIC     ! WILLIG   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 DTA-VORSATZ.
           10 SATZ-KZ-VS             PIC  X(001).
           10 BABS                   PIC  X(027).
           10 BERST-TTMMJJJJ.
               15 BERST-TT           PIC  9(002).
               15 BERST-MM           PIC  9(002).
               15 BERST-JJJJ         PIC  9(004).
           10 BSTAND-TTMMJJJJ.
               15 BSTAND-TT          PIC  9(002).
               15 BSTAND-MM          PIC  9(002).
               15 BSTAND-JJJJ        PIC  9(004).
           10 BEMPF                  PIC  X(027).
           10 BINHALT                PIC  X(020).
           10 WRGKZN                 PIC  X(003).
      *----Variable Satzlänge von 94Bytes------------------------------


       01 DTA-DATENSATZ.
           10 SATZ-KZ                PIC  X(001).
           10 MA-NR                  PIC  X(010).
           10 UEBETREUER-VB-NR       PIC  X(010).
           10 DBETREUER-VB-NR        PIC  X(010).
           10 DIRBETREUER-VB-NR      PIC  X(010).
           10 FUNKTION               PIC  X(002).
           10 EROEFF-DAT             PIC  9(008).
           10 SCHLIE-DAT             PIC  9(008).
           10 ANREDE                 PIC  9(001).
           10 ZUNAME                 PIC  X(040).
           10 VORNAME                PIC  X(020).
           10 TITEL-AKAD             PIC  X(014).
           10 TITEL-ADEL             PIC  X(015).
           10 AUSL-KZ                PIC  X(003).
           10 PLZ                    PIC  X(005).
180607     10 ORT                    PIC  X(040).
180607     10 STR                    PIC  X(040).
           10 GEBDAT                 PIC  9(008).
180607     10 TELEFON                PIC  X(026).
180607     10 FAX-NR                 PIC  X(026).
           10 STORES1                PIC S9(004)V9  PACKED-DECIMAL.
           10 BKTOINHORT             PIC  X(025).
           10 BKTONR                 PIC S9(011)    PACKED-DECIMAL.
           10 GELDINST               PIC  X(022).
           10 BLZ                    PIC S9(008)    BINARY.
           10 STRUK-DIR              PIC  X(004).
           10 STRUK-STUFE            PIC  X(004).
           10 KNR-ST-TYP             PIC S9(004)    BINARY.
           10 VBP-NR                 PIC S9(004)    BINARY.
           10 STUFEN-TEXT            PIC  X(004).
180607     10 HANDY                  PIC  X(026).
           10 EMAIL                  PIC  X(083).
           10 URL                    PIC  X(083).
           10 UMST-NR                PIC  X(015).
180607     10 REGISTER-NR            PIC  X(030).
180607     10 AM-AGENT-NR            PIC S9(009)    BINARY.
180607     10 VERRECHNKTO-KZ         PIC  X(001).
180607     10 MA-ALLFI-STATUS        PIC  X(001).
170408     10 MA-KEINE-ERF-PROV      PIC  X(001).
270508     10 STUFEN-TEXT-LANG       PIC  X(040).
280111     10 MA-ABSCHL-VERM         PIC  X(001).
061211     10 MA-CKV-WECHSEL         PIC  X(001).
061211     10 MA-CKV-ERFPROV         PIC  X(001).
160212     10 MA-SCHW-PARTNER        PIC  X(001).
040313     10 BV-IBAN                PIC  X(034).
040313     10 BV-BIC                 PIC  X(011).
100516     10 STRL-STA               PIC  9(004).
100516     10 STRL-STA-TXT           PIC  X(025).
110520     10 BKTOINHORT-LANG        PIC  X(040).
110520     10 GELDINST-LANG          PIC  X(040).

      *---- Satzlänge von 735 Bytes -----------------------------------


       01 DTA-NACHSATZ.
           10 SATZ-KZ-NS             PIC  X(001).
           10 FSAETZE                PIC S9(007)    PACKED-DECIMAL.
           10 FSUMME                 PIC S9(015)V99 PACKED-DECIMAL
                                     OCCURS 5.
      *----Variable Satzlänge von 50Bytes------------------------------

      *-------------------- COPY-MEMBER DRVB0332 -----------------------


