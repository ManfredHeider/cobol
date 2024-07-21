      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5139.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5139.CPv  $
      *
      *   Rev 2.2   Jul 01 2016 10:14:50   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.1   Jun 06 2016 14:11:18   i13az62
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.0   Apr 26 2016 10:21:14   i13az62
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5043   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5139
      *
      *     GENERIERUNG      :  01.07.2016/09:54:52
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00263!26.04.16!2.1  ! NEUERSTELLUNG             ! KUBE
      * AMA00263!01.07.16!2.2  ! ÄNDERUNG DRMA5141         ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5139.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VG                     pic 9(4).
              07 DRMA5141.
                 09 FIRMA               pic XXX.
                 09 TZ-01               pic X.
                 09 STATUS-KZN          pic X.
                 09 TZ-02               pic X.
                 09 WRGKZN              pic XXX.
                 09 TZ-03               pic X.
                 09 KONTOROLLE          pic XXX.
                 09 TZ-04               pic X.
                 09 KONTO               pic X(12).
                 09 TZ-05               pic X.
                 09 BUCHPERIODE         pic X(6).
                 09 TZ-06               pic X.
                 09 BELEGDATUM          pic X(8).
                 09 TZ-07               pic X.
                 09 BUCHDAT             pic X(8).
                 09 TZ-08               pic X.
                 09 BELEGART            pic XXX.
                 09 TZ-09               pic X.
                 09 BELEGNR             pic X(7).
                 09 TZ-10               pic X.
                 09 BNGBETR             pic 9(14)V99.
                 09 TZ-11               pic X.
                 09 SH-KZN              pic X.
                 09 TZ-12               pic X.
                 09 TEXTSCHLUESSEL      pic XXX.
                 09 TZ-13               pic X.
                 09 BUCHTEXT            pic X(40).
                 09 TZ-14               pic X.
                 09 GEGENKONTOROLLE     pic XXX.
                 09 TZ-15               pic X.
                 09 GEGENKONTO          pic X(12).
                 09 TZ-16               pic X.
                 09 AUSSENSTELLE        pic X(8).
                 09 TZ-17               pic X.
                 09 KOSTENSTELLE        pic X(6).
                 09 TZ-18               pic X.
                 09 SPARTE              pic X(8).
                 09 TZ-19               pic X.
                 09 PROJEKT             pic X(8).
                 09 TZ-20               pic X.
                 09 ZEICHNUNGSJAHR      pic X(4).
                 09 TZ-21               pic X.
                 09 FREMDWAEHRUNG       pic XXX.
                 09 TZ-22               pic X.
                 09 BNGBETRHW           pic 9(14)V99.
                 09 TZ-23               pic X.
                 09 EXTKONTO            pic X(12).
                 09 TZ-24               pic X.
                 09 AUFWANDSKONTOINFO   pic X(12).
                 09 TZ-25               pic X.
                 09 FAELLIGAM           pic X(8).
                 09 TZ-26               pic X.
                 09 VERSNR              pic X(17).
                 09 TZ-27               pic X.
                 09 RECHNUNGSNR         pic X(15).
                 09 TZ-28               pic X.
                 09 SCHADENNR           occurs 20
                                        pic X.
                 09 TZ-29               pic X.
                 09 ERHEBUNGVON         pic X(8).
                 09 TZ-30               pic X.
                 09 ERHEBUNGBIS         pic X(8).
                 09 TZ-31               pic X.
                 09 VERMITTLER          pic X(12).
                 09 TZ-32               pic X.
                 09 FUEHRENDERVERSICHERER
                                        pic X(12).
                 09 TZ-33               pic X.
                 09 VERSICHERER         pic X(12).
                 09 TZ-34               pic X.
                 09 VRANTEIL            pic X(5).
                 09 TZ-35               pic X.
                 09 REFERENZ            pic X(17).
                 09 TZ-36               pic X.
                 09 KZKFZ               pic X(12).
                 09 TZ-37               pic X.
                 09 INKASSOART          pic XXX.
                 09 TZ-38               pic X.
                 09 ZAHLWEISE           pic XXX.
                 09 TZ-39               pic X.
                 09 VERSSTEUERSCHLUESSEL
                                        pic X.
                 09 TZ-40               pic X.
                 09 VERSSTEUERSATZ      pic X(5).
                 09 TZ-41               pic X.
                 09 VERSSTEUER          pic 9(16).
                 09 TZ-42               pic X.
                 09 FEUERSCHUTZSTEUERSATZ
                                        pic X(5).
                 09 TZ-43               pic X.
                 09 FEUERSCHUTZSTEUER   pic 9(16).
                 09 TZ-44               pic X.
                 09 NETTOPRAEMIE        pic 9(16).
                 09 TZ-45               pic X.
                 09 GEBUEHR             pic 9(16).
                 09 TZ-46               pic X.
                 09 PROVISIONSSATZ      pic X(5).
                 09 TZ-47               pic X.
                 09 PROVISION           pic 9(16).
                 09 TZ-48               pic X.
                 09 PROVISIONSBASIS     pic X.
                 09 TZ-49               pic X.
                 09 PROVISIONSBASISBETRAG
                                        pic 9(16).
                 09 TZ-50               pic X.
                 09 COURTAGESATZ        pic X(5).
                 09 TZ-51               pic X.
                 09 COURTAGE            pic 9(16).
                 09 TZ-52               pic X.
                 09 COURTAGEBASIS       pic X.
                 09 TZ-53               pic X.
                 09 KTY-BUCHUNGS-TYP    pic 9(4).
                 09 KNR-BUCHUNGS-TYP    pic 9(4).
                 09 KTY-VERARB-TYP      pic 9(4).
                 09 KNR-VERARB-TYP      pic 9(4).
                 09 TZ-54               pic X.
                 09 PROVISIONSATZFUEHRENDE
                                        pic X(5).
                 09 TZ-55               pic X.
                 09 REFERENZ-NR         pic X(16).
                 09 TZ-56               pic X.
                 09 KTY-LEIST-ART       pic 9(4).
                 09 KNR-LEIST-ART       pic 9(4).
                 09 UST-SATZ            pic 999V99.
                 09 RATENZUSCHLAGREST   pic 999.
                 09 TZ-57               pic X.
                 09 BUCHUNGSKZINTERN    pic XX.
                 09 TZ-58               pic X.
                 09 BUCHUNGSVERMERK     pic XX.
                 09 TZ-59               pic X.
                 09 BELEGDATUMALT       pic X(8).
                 09 TZ-60               pic X.
                 09 OFFENERPOSTEN       pic X(7).
                 09 TZ-61               pic X.
                 09 POSTENZUORDNUNG     pic XXX.
                 09 TZ-62               pic X.
                 09 INTLFDNUMMER        occurs 25
                                        pic X.
                 09 TZ-63               pic X.
                 09 LFDNUMMER           occurs 25
                                        pic X.
                 09 TZ-64               pic X.
                 09 VERSNEHMER          pic X(12).
                 09 TZ-65               pic X.
                 09 BEITRAGSZAHLER      pic X(12).
                 09 TZ-66               pic X.
                 09 PROVISIONSART       pic X.
                 09 TZ-67               pic X.
                 09 SOLLSTELLUNGSART    pic X.
                 09 TZ-68               pic X.
                 09 HERKUNFTSKENNUNG    pic XX.
                 09 TZ-69               pic X.
                 09 LEVSTOP             pic 9.
                 09 TZ-70               pic X.
                 09 ZAHLSTOP            pic 9.
                 09 TZ-71               pic X.
                 09 MAHNSTOP            pic 9.
                 09 TZ-72               pic X.
                 09 BUCHTEXT2           pic X(40).
                 09 TZ-73               pic X.
                 09 ANZAHLMITARBEITER   pic X(7).
                 09 TZ-74               pic X.
                 09 HERKUNFTSLAND       pic X(8).
                 09 TZ-75               pic X.
                 09 GESELLSCHAFT        pic X(8).
                 09 TZ-76               pic X.
                 09 PARTNERNR           pic X(8).
                 09 TZ-77               pic X.
                 09 EXTERNBELNR         pic X(7).
                 09 TZ-78               pic X.
                 09 BRUTTO-BETRAG       pic 9(14)V99.
                 09 TZ-79               pic X.
                 09 UST                 pic 9(14)V99.
                 09 TZ-80               pic X.
                 09 UNIVERSALBETRAG3    pic 9(16).
                 09 TZ-81               pic X.
                 09 UNIVERSALDATUM1     pic X(8).
                 09 TZ-82               pic X.
                 09 UNIVERSALDATUM2     pic X(8).
                 09 TZ-83               pic X.
                 09 FINSTATVERARBDATUM  pic X(8).
                 09 TZ-84               pic X.
                 09 VBNR                pic X(7).
                 09 PRZ                 pic X.
                 09 UNIVERSALTEXT1REST  occurs 32
                                        pic X.
                 09 TZ-85               pic X.
                 09 KTO-KZN             pic X.
                 09 UNIVERSALTEXT2REST  occurs 39
                                        pic X.
                 09 TZ-86               pic X.
                 09 SUSISKZ             pic X.
                 09 UST-BERECH-KZN      pic X.
                 09 KORREKTUR-ZIEL-STATUS
                                        pic X.
                 09 UST-KORREKTUR-KZN   pic X.
                 09 UST-PFLICHTIG-KZN   pic X.
                 09 STEUERJAHR          pic X(4).
                 09 FILLER              pic X(4).
                 09 BEZUGSJAHR          pic X(4).
                 09 FILLER              pic X(4).
                 09 ABR-NR-KORR         pic X(15).
                 09 UNIVERSALTEXT3REST  pic X(4).
                 09 TZ-87               pic X.
                 09 ANLAGEDATUM         pic X(8).
                 09 TZ-88               pic X.
                 09 ANLAGESACHBEARBEITER
                                        pic X(10).
      *
      **************************************************************

