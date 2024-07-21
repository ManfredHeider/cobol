      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5141.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5141.CPv  $
      *
      *   Rev 2.4   Jul 01 2016 10:15:02   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.3   Jun 03 2016 11:00:56   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.2   May 31 2016 09:54:56   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.1   May 18 2016 11:21:50   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.0   Apr 26 2016 14:11:10   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: SST Finanzbuchhaltung
      *
      *     SOURCE-NAME      :  DRMA5141
      *
      *     GENERIERUNG      :  01.07.2016/09:52:37
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00263!26.04.16!2.1  ! NEUERSTELLUNG;            ! WOLFF
      *         !        !     ! FELDNAMEN AUF UNSERE      !
      *         !        !     ! NAMEN ANGEPASST           !
      * AMA00263!31.05.16!2.2  ! ÄNDERUNG DEMA0429         ! WOLFF
      * AMA00263!03.06.15!2.3  ! ÄNDERUNG REFERENZ-ID IN   ! WOLFF
      *         !        !     ! REFERENZ-NR               !
      * AMA00263!01.07.16!2.6  ! VBNR 8-STELLIG AUFGETEILT ! WOLFF
      *         !        !     ! IN VBNR 7-STELLIG UND PRZ !
      *         !        !     ! 1-STELLIG                 !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5141.
              07 FIRMA                  pic XXX.
              07 TZ-01                  pic X.
              07 STATUS-KZN             pic X.
              07 TZ-02                  pic X.
              07 WRGKZN                 pic XXX.
              07 TZ-03                  pic X.
              07 KONTOROLLE             pic XXX.
              07 TZ-04                  pic X.
              07 KONTO                  pic X(12).
              07 TZ-05                  pic X.
              07 BUCHPERIODE            pic X(6).
              07 TZ-06                  pic X.
              07 BELEGDATUM             pic X(8).
              07 TZ-07                  pic X.
              07 BUCHDAT                pic X(8).
              07 TZ-08                  pic X.
              07 BELEGART               pic XXX.
              07 TZ-09                  pic X.
              07 BELEGNR                pic X(7).
              07 TZ-10                  pic X.
              07 BNGBETR                pic 9(14)V99.
              07 TZ-11                  pic X.
              07 SH-KZN                 pic X.
              07 TZ-12                  pic X.
              07 TEXTSCHLUESSEL         pic XXX.
              07 TZ-13                  pic X.
              07 BUCHTEXT               pic X(40).
              07 TZ-14                  pic X.
              07 GEGENKONTOROLLE        pic XXX.
              07 TZ-15                  pic X.
              07 GEGENKONTO             pic X(12).
              07 TZ-16                  pic X.
              07 AUSSENSTELLE           pic X(8).
              07 TZ-17                  pic X.
              07 KOSTENSTELLE           pic X(6).
              07 TZ-18                  pic X.
              07 SPARTE                 pic X(8).
              07 TZ-19                  pic X.
              07 PROJEKT                pic X(8).
              07 TZ-20                  pic X.
              07 ZEICHNUNGSJAHR         pic X(4).
              07 TZ-21                  pic X.
              07 FREMDWAEHRUNG          pic XXX.
              07 TZ-22                  pic X.
              07 BNGBETRHW              pic 9(14)V99.
              07 TZ-23                  pic X.
              07 EXTKONTO               pic X(12).
              07 TZ-24                  pic X.
              07 AUFWANDSKONTOINFO      pic X(12).
              07 TZ-25                  pic X.
              07 FAELLIGAM              pic X(8).
              07 TZ-26                  pic X.
              07 VERSNR                 pic X(17).
              07 TZ-27                  pic X.
              07 RECHNUNGSNR            pic X(15).
              07 TZ-28                  pic X.
              07 SCHADENNR              occurs 20
                                        pic X.
              07 TZ-29                  pic X.
              07 ERHEBUNGVON            pic X(8).
              07 TZ-30                  pic X.
              07 ERHEBUNGBIS            pic X(8).
              07 TZ-31                  pic X.
              07 VERMITTLER             pic X(12).
              07 TZ-32                  pic X.
              07 FUEHRENDERVERSICHERER  pic X(12).
              07 TZ-33                  pic X.
              07 VERSICHERER            pic X(12).
              07 TZ-34                  pic X.
              07 VRANTEIL               pic X(5).
              07 TZ-35                  pic X.
              07 REFERENZ               pic X(17).
              07 TZ-36                  pic X.
              07 KZKFZ                  pic X(12).
              07 TZ-37                  pic X.
              07 INKASSOART             pic XXX.
              07 TZ-38                  pic X.
              07 ZAHLWEISE              pic XXX.
              07 TZ-39                  pic X.
              07 VERSSTEUERSCHLUESSEL   pic X.
              07 TZ-40                  pic X.
              07 VERSSTEUERSATZ         pic X(5).
              07 TZ-41                  pic X.
              07 VERSSTEUER             pic 9(16).
              07 TZ-42                  pic X.
              07 FEUERSCHUTZSTEUERSATZ  pic X(5).
              07 TZ-43                  pic X.
              07 FEUERSCHUTZSTEUER      pic 9(16).
              07 TZ-44                  pic X.
              07 NETTOPRAEMIE           pic 9(16).
              07 TZ-45                  pic X.
              07 GEBUEHR                pic 9(16).
              07 TZ-46                  pic X.
              07 PROVISIONSSATZ         pic X(5).
              07 TZ-47                  pic X.
              07 PROVISION              pic 9(16).
              07 TZ-48                  pic X.
              07 PROVISIONSBASIS        pic X.
              07 TZ-49                  pic X.
              07 PROVISIONSBASISBETRAG  pic 9(16).
              07 TZ-50                  pic X.
              07 COURTAGESATZ           pic X(5).
              07 TZ-51                  pic X.
              07 COURTAGE               pic 9(16).
              07 TZ-52                  pic X.
              07 COURTAGEBASIS          pic X.
              07 TZ-53                  pic X.
              07 KTY-BUCHUNGS-TYP       pic 9(4).
              07 KNR-BUCHUNGS-TYP       pic 9(4).
              07 KTY-VERARB-TYP         pic 9(4).
              07 KNR-VERARB-TYP         pic 9(4).
              07 TZ-54                  pic X.
              07 PROVISIONSATZFUEHRENDE pic X(5).
              07 TZ-55                  pic X.
              07 REFERENZ-NR            pic X(16).
              07 TZ-56                  pic X.
              07 KTY-LEIST-ART          pic 9(4).
              07 KNR-LEIST-ART          pic 9(4).
              07 UST-SATZ               pic 999V99.
              07 RATENZUSCHLAGREST      pic 999.
              07 TZ-57                  pic X.
              07 BUCHUNGSKZINTERN       pic XX.
              07 TZ-58                  pic X.
              07 BUCHUNGSVERMERK        pic XX.
              07 TZ-59                  pic X.
              07 BELEGDATUMALT          pic X(8).
              07 TZ-60                  pic X.
              07 OFFENERPOSTEN          pic X(7).
              07 TZ-61                  pic X.
              07 POSTENZUORDNUNG        pic XXX.
              07 TZ-62                  pic X.
              07 INTLFDNUMMER           occurs 25
                                        pic X.
              07 TZ-63                  pic X.
              07 LFDNUMMER              occurs 25
                                        pic X.
              07 TZ-64                  pic X.
              07 VERSNEHMER             pic X(12).
              07 TZ-65                  pic X.
              07 BEITRAGSZAHLER         pic X(12).
              07 TZ-66                  pic X.
              07 PROVISIONSART          pic X.
              07 TZ-67                  pic X.
              07 SOLLSTELLUNGSART       pic X.
              07 TZ-68                  pic X.
              07 HERKUNFTSKENNUNG       pic XX.
              07 TZ-69                  pic X.
              07 LEVSTOP                pic 9.
              07 TZ-70                  pic X.
              07 ZAHLSTOP               pic 9.
              07 TZ-71                  pic X.
              07 MAHNSTOP               pic 9.
              07 TZ-72                  pic X.
              07 BUCHTEXT2              pic X(40).
              07 TZ-73                  pic X.
              07 ANZAHLMITARBEITER      pic X(7).
              07 TZ-74                  pic X.
              07 HERKUNFTSLAND          pic X(8).
              07 TZ-75                  pic X.
              07 GESELLSCHAFT           pic X(8).
              07 TZ-76                  pic X.
              07 PARTNERNR              pic X(8).
              07 TZ-77                  pic X.
              07 EXTERNBELNR            pic X(7).
              07 TZ-78                  pic X.
              07 BRUTTO-BETRAG          pic 9(14)V99.
              07 TZ-79                  pic X.
              07 UST                    pic 9(14)V99.
              07 TZ-80                  pic X.
              07 UNIVERSALBETRAG3       pic 9(16).
              07 TZ-81                  pic X.
              07 UNIVERSALDATUM1        pic X(8).
              07 TZ-82                  pic X.
              07 UNIVERSALDATUM2        pic X(8).
              07 TZ-83                  pic X.
              07 FINSTATVERARBDATUM     pic X(8).
              07 TZ-84                  pic X.
              07 VBNR                   pic X(7).
              07 PRZ                    pic X.
              07 UNIVERSALTEXT1REST     occurs 32
                                        pic X.
              07 TZ-85                  pic X.
              07 KTO-KZN                pic X.
              07 UNIVERSALTEXT2REST     occurs 39
                                        pic X.
              07 TZ-86                  pic X.
              07 SUSISKZ                pic X.
              07 UST-BERECH-KZN         pic X.
              07 KORREKTUR-ZIEL-STATUS  pic X.
              07 UST-KORREKTUR-KZN      pic X.
              07 UST-PFLICHTIG-KZN      pic X.
              07 STEUERJAHR             pic X(4).
              07 FILLER                 pic X(4).
              07 BEZUGSJAHR             pic X(4).
              07 FILLER                 pic X(4).
              07 ABR-NR-KORR            pic X(15).
              07 UNIVERSALTEXT3REST     pic X(4).
              07 TZ-87                  pic X.
              07 ANLAGEDATUM            pic X(8).
              07 TZ-88                  pic X.
              07 ANLAGESACHBEARBEITER   pic X(10).
      *
      **************************************************************

