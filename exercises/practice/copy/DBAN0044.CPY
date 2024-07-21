      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0044.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DBAN0044.CPv  $
      *
      *   Rev 2.7   Mar 06 2018 08:50:36   t13ae23
      *AAN01362: Entfernen IDD für Leben für die Dialoge AN01/AN02/
      *AN03
      *
      *   Rev 2.6   Dec 08 2017 15:36:16   t13ae23
      *AAN01352: IDD-Geeignetheitsprüfung inkl. Auswertung
      *
      *   Rev 2.5   Nov 04 2013 15:58:38   t13ae23
      *AAN01073: Projekt SEPA - Entfernen SEPA-Mandat -Kz
      *
      *   Rev 2.4   Jun 21 2013 13:22:44   t13ae34
      *AAN01073: Projekt SEPA - Kennzeichen Mandat
      *
      *   Rev 2.3   28 Aug 2007 14:56:42   t13ae23
      *AAN00750: Neuer Tarif 4RG Ges. 20 AM-Leben
      *
      *   Rev 2.2   Mar 07 2007 19:50:04   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Mar 07 2007 19:01:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Dec 2006 13:01:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-31)
      *   MAPSET NAME   BAN0044
      *   DATE CREATED  06/03/2018
      *   TIME CREATED  08:47:30

      *  INPUT DATA FOR MAP BAN0044
         01 BAN0044I.
            03 FILLER                         PIC X(12).
            03 TVBNRL                         PIC S9(4) COMP.
            03 TVBNRF                         PIC X.
            03 FILLER REDEFINES TVBNRF.
               05 TVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNRI                         PIC X(11).
            03 AVBNRL                         PIC S9(4) COMP.
            03 AVBNRF                         PIC X.
            03 FILLER REDEFINES AVBNRF.
               05 AVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNRI                         PIC X(7).
            03 AVBPRZL                        PIC S9(4) COMP.
            03 AVBPRZF                        PIC X.
            03 FILLER REDEFINES AVBPRZF.
               05 AVBPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPRZI                        PIC X(1).
            03 ADIRL                          PIC S9(4) COMP.
            03 ADIRF                          PIC X.
            03 FILLER REDEFINES ADIRF.
               05 ADIRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRI                          PIC X(3).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER REDEFINES AVBNAMEF.
               05 AVBNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(20).
            03 TVORSCL                        PIC S9(4) COMP.
            03 TVORSCF                        PIC X.
            03 FILLER REDEFINES TVORSCF.
               05 TVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVORSCI                        PIC X(7).
            03 AVORSCL                        PIC S9(4) COMP.
            03 AVORSCF                        PIC X.
            03 FILLER REDEFINES AVORSCF.
               05 AVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVORSCI                        PIC X(17).
            03 TVMNRL                         PIC S9(4) COMP.
            03 TVMNRF                         PIC X.
            03 FILLER REDEFINES TVMNRF.
               05 TVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVMNRI                         PIC X(11).
            03 AVMNRL                         PIC S9(4) COMP.
            03 AVMNRF                         PIC X.
            03 FILLER REDEFINES AVMNRF.
               05 AVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNRI                         PIC X(7).
            03 AVMPRZL                        PIC S9(4) COMP.
            03 AVMPRZF                        PIC X.
            03 FILLER REDEFINES AVMPRZF.
               05 AVMPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVMPRZI                        PIC X(1).
            03 AVMNAMEL                       PIC S9(4) COMP.
            03 AVMNAMEF                       PIC X.
            03 FILLER REDEFINES AVMNAMEF.
               05 AVMNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNAMEI                       PIC X(20).
            03 TVBZEIL                        PIC S9(4) COMP.
            03 TVBZEIF                        PIC X.
            03 FILLER REDEFINES TVBZEIF.
               05 TVBZEIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVBZEII                        PIC X(13).
            03 AVBZEIL                        PIC S9(4) COMP.
            03 AVBZEIF                        PIC X.
            03 FILLER REDEFINES AVBZEIF.
               05 AVBZEIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVBZEII                        PIC X(7).
            03 TPINL                          PIC S9(4) COMP.
            03 TPINF                          PIC X.
            03 FILLER REDEFINES TPINF.
               05 TPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPINI                          PIC X(10).
            03 ESPARTL                        PIC S9(4) COMP.
            03 ESPARTF                        PIC X.
            03 FILLER REDEFINES ESPARTF.
               05 ESPARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESPARTI                        PIC X(1).
            03 EPINL                          PIC S9(4) COMP.
            03 EPINF                          PIC X.
            03 FILLER REDEFINES EPINF.
               05 EPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPINI                          PIC X(7).
            03 EPINPRL                        PIC S9(4) COMP.
            03 EPINPRF                        PIC X.
            03 FILLER REDEFINES EPINPRF.
               05 EPINPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPINPRI                        PIC X(2).
            03 TKDNAML                        PIC S9(4) COMP.
            03 TKDNAMF                        PIC X.
            03 FILLER REDEFINES TKDNAMF.
               05 TKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDNAMI                        PIC X(10).
            03 EKDNAML                        PIC S9(4) COMP.
            03 EKDNAMF                        PIC X.
            03 FILLER REDEFINES EKDNAMF.
               05 EKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDNAMI                        PIC X(26).
            03 TKDVNAL                        PIC S9(4) COMP.
            03 TKDVNAF                        PIC X.
            03 FILLER REDEFINES TKDVNAF.
               05 TKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDVNAI                        PIC X(10).
            03 EKDVNAL                        PIC S9(4) COMP.
            03 EKDVNAF                        PIC X.
            03 FILLER REDEFINES EKDVNAF.
               05 EKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDVNAI                        PIC X(26).
            03 TPLZL                          PIC S9(4) COMP.
            03 TPLZF                          PIC X.
            03 FILLER REDEFINES TPLZF.
               05 TPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPLZI                          PIC X(10).
            03 EPLZL                          PIC S9(4) COMP.
            03 EPLZF                          PIC X.
            03 FILLER REDEFINES EPLZF.
               05 EPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPLZI                          PIC X(5).
            03 EKDORTL                        PIC S9(4) COMP.
            03 EKDORTF                        PIC X.
            03 FILLER REDEFINES EKDORTF.
               05 EKDORTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDORTI                        PIC X(21).
            03 EAUSLL                         PIC S9(4) COMP.
            03 EAUSLF                         PIC X.
            03 FILLER REDEFINES EAUSLF.
               05 EAUSLA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAUSLI                         PIC X(3).
            03 TGEBURL                        PIC S9(4) COMP.
            03 TGEBURF                        PIC X.
            03 FILLER REDEFINES TGEBURF.
               05 TGEBURA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TGEBURI                        PIC X(25).
            03 EGEBURL                        PIC S9(4) COMP.
            03 EGEBURF                        PIC X.
            03 FILLER REDEFINES EGEBURF.
               05 EGEBURA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGEBURI                        PIC X(4).
            03 ETARIFL                        PIC S9(4) COMP.
            03 ETARIFF                        PIC X.
            03 FILLER REDEFINES ETARIFF.
               05 ETARIFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ETARIFI                        PIC X(15).
            03 ESUMMEL                        PIC S9(4) COMP.
            03 ESUMMEF                        PIC X.
            03 FILLER REDEFINES ESUMMEF.
               05 ESUMMEA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESUMMEI                        PIC X(9).
            03 TVEBEGL                        PIC S9(4) COMP.
            03 TVEBEGF                        PIC X.
            03 FILLER REDEFINES TVEBEGF.
               05 TVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVEBEGI                        PIC X(25).
            03 EVEBEGL                        PIC S9(4) COMP.
            03 EVEBEGF                        PIC X.
            03 FILLER REDEFINES EVEBEGF.
               05 EVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVEBEGI                        PIC X(6).
            03 ERBEGDL                        PIC S9(4) COMP.
            03 ERBEGDF                        PIC X.
            03 FILLER REDEFINES ERBEGDF.
               05 ERBEGDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ERBEGDI                        PIC X(6).
            03 EBZENDL                        PIC S9(4) COMP.
            03 EBZENDF                        PIC X.
            03 FILLER REDEFINES EBZENDF.
               05 EBZENDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBZENDI                        PIC X(6).
            03 TBEITRL                        PIC S9(4) COMP.
            03 TBEITRF                        PIC X.
            03 FILLER REDEFINES TBEITRF.
               05 TBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TBEITRI                        PIC X(25).
            03 EBEITRL                        PIC S9(4) COMP.
            03 EBEITRF                        PIC X.
            03 FILLER REDEFINES EBEITRF.
               05 EBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBEITRI                        PIC X(9).
            03 EZAHLWL                        PIC S9(4) COMP.
            03 EZAHLWF                        PIC X.
            03 FILLER REDEFINES EZAHLWF.
               05 EZAHLWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EZAHLWI                        PIC X(1).
            03 TDIREKL                        PIC S9(4) COMP.
            03 TDIREKF                        PIC X.
            03 FILLER REDEFINES TDIREKF.
               05 TDIREKA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TDIREKI                        PIC X(25).
            03 EDVERSL                        PIC S9(4) COMP.
            03 EDVERSF                        PIC X.
            03 FILLER REDEFINES EDVERSF.
               05 EDVERSA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EDVERSI                        PIC X(1).
            03 EKOLSTL                        PIC S9(4) COMP.
            03 EKOLSTF                        PIC X.
            03 FILLER REDEFINES EKOLSTF.
               05 EKOLSTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKOLSTI                        PIC X(1).
            03 AEINHL                         PIC S9(4) COMP.
            03 AEINHF                         PIC X.
            03 FILLER REDEFINES AEINHF.
               05 AEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AEINHI                         PIC X(7).
            03 EEINHL                         PIC S9(4) COMP.
            03 EEINHF                         PIC X.
            03 FILLER REDEFINES EEINHF.
               05 EEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EEINHI                         PIC X(7).
            03 TPRODL                         PIC S9(4) COMP.
            03 TPRODF                         PIC X.
            03 FILLER REDEFINES TPRODF.
               05 TPRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI                         PIC X(25).
            03 APRODL                         PIC S9(4) COMP.
            03 APRODF                         PIC X.
            03 FILLER REDEFINES APRODF.
               05 APRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 APRODI                         PIC X(3).
            03 TEIGENL                        PIC S9(4) COMP.
            03 TEIGENF                        PIC X.
            03 FILLER REDEFINES TEIGENF.
               05 TEIGENA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TEIGENI                        PIC X(25).
            03 EEIGENL                        PIC S9(4) COMP.
            03 EEIGENF                        PIC X.
            03 FILLER REDEFINES EEIGENF.
               05 EEIGENA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEIGENI                        PIC X(1).
            03 AEIGENL                        PIC S9(4) COMP.
            03 AEIGENF                        PIC X.
            03 FILLER REDEFINES AEIGENF.
               05 AEIGENA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AEIGENI                        PIC X(45).
            03 TBERPRL                        PIC S9(4) COMP.
            03 TBERPRF                        PIC X.
            03 FILLER REDEFINES TBERPRF.
               05 TBERPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TBERPRI                        PIC X(32).
            03 EBERPRL                        PIC S9(4) COMP.
            03 EBERPRF                        PIC X.
            03 FILLER REDEFINES EBERPRF.
               05 EBERPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBERPRI                        PIC X(1).
            03 TANARTL                        PIC S9(4) COMP.
            03 TANARTF                        PIC X.
            03 FILLER REDEFINES TANARTF.
               05 TANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TANARTI                        PIC X(7).
            03 EANARTL                        PIC S9(4) COMP.
            03 EANARTF                        PIC X.
            03 FILLER REDEFINES EANARTF.
               05 EANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EANARTI                        PIC X(1).
            03 TAUFNDL                        PIC S9(4) COMP.
            03 TAUFNDF                        PIC X.
            03 FILLER REDEFINES TAUFNDF.
               05 TAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TAUFNDI                        PIC X(6).
            03 EAUFNDL                        PIC S9(4) COMP.
            03 EAUFNDF                        PIC X.
            03 FILLER REDEFINES EAUFNDF.
               05 EAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAUFNDI                        PIC X(6).
            03 TPESTRL                        PIC S9(4) COMP.
            03 TPESTRF                        PIC X.
            03 FILLER REDEFINES TPESTRF.
               05 TPESTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPESTRI                        PIC X(6).
            03 EPESTRL                        PIC S9(4) COMP.
            03 EPESTRF                        PIC X.
            03 FILLER REDEFINES EPESTRF.
               05 EPESTRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPESTRI                        PIC X(6).
            03 TSTATIL                        PIC S9(4) COMP.
            03 TSTATIF                        PIC X.
            03 FILLER REDEFINES TSTATIF.
               05 TSTATIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TSTATII                        PIC X(6).
            03 ESTATIL                        PIC S9(4) COMP.
            03 ESTATIF                        PIC X.
            03 FILLER REDEFINES ESTATIF.
               05 ESTATIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTATII                        PIC X(6).
            03 TSTATUL                        PIC S9(4) COMP.
            03 TSTATUF                        PIC X.
            03 FILLER REDEFINES TSTATUF.
               05 TSTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TSTATUI                        PIC X(6).
            03 ESTATUL                        PIC S9(4) COMP.
            03 ESTATUF                        PIC X.
            03 FILLER REDEFINES ESTATUF.
               05 ESTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTATUI                        PIC X(2).
            03 ASTATUL                        PIC S9(4) COMP.
            03 ASTATUF                        PIC X.
            03 FILLER REDEFINES ASTATUF.
               05 ASTATUA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASTATUI                        PIC X(12).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0044
         01 BAN0044O REDEFINES BAN0044I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 AVBNRH                         PIC X.
            03 AVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVBPRZH                        PIC X.
            03 AVBPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIRH                          PIC X.
            03 ADIRO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AVBNAMEH                       PIC X.
            03 AVBNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVORSCH                        PIC X.
            03 TVORSCO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVORSCH                        PIC X.
            03 AVORSCO                        PIC X(17).
            03 FILLER                         PIC X(3).
            03 TVMNRH                         PIC X.
            03 TVMNRO                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 AVMNRH                         PIC X.
            03 AVMNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVMPRZH                        PIC X.
            03 AVMPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AVMNAMEH                       PIC X.
            03 AVMNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVBZEIH                        PIC X.
            03 TVBZEIO                        PIC X(13).
            03 FILLER                         PIC X(3).
            03 AVBZEIH                        PIC X.
            03 AVBZEIO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPINH                          PIC X.
            03 TPINO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 ESPARTH                        PIC X.
            03 ESPARTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPINH                          PIC X.
            03 EPINO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 EPINPRH                        PIC X.
            03 EPINPRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TKDNAMH                        PIC X.
            03 TKDNAMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDNAMH                        PIC X.
            03 EKDNAMO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKDVNAH                        PIC X.
            03 TKDVNAO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDVNAH                        PIC X.
            03 EKDVNAO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 TPLZH                          PIC X.
            03 TPLZO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPLZH                          PIC X.
            03 EPLZO                          PIC X(5).
            03 FILLER                         PIC X(3).
            03 EKDORTH                        PIC X.
            03 EKDORTO                        PIC X(21).
            03 FILLER                         PIC X(3).
            03 EAUSLH                         PIC X.
            03 EAUSLO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 TGEBURH                        PIC X.
            03 TGEBURO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EGEBURH                        PIC X.
            03 EGEBURO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ETARIFH                        PIC X.
            03 ETARIFO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 ESUMMEH                        PIC X.
            03 ESUMMEO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TVEBEGH                        PIC X.
            03 TVEBEGO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EVEBEGH                        PIC X.
            03 EVEBEGO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ERBEGDH                        PIC X.
            03 ERBEGDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EBZENDH                        PIC X.
            03 EBZENDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TBEITRH                        PIC X.
            03 TBEITRO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EBEITRH                        PIC X.
            03 EBEITRO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 EZAHLWH                        PIC X.
            03 EZAHLWO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TDIREKH                        PIC X.
            03 TDIREKO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EDVERSH                        PIC X.
            03 EDVERSO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EKOLSTH                        PIC X.
            03 EKOLSTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEINHH                         PIC X.
            03 AEINHO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 EEINHH                         PIC X.
            03 EEINHO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPRODH                         PIC X.
            03 TPRODO                         PIC X(25).
            03 FILLER                         PIC X(3).
            03 APRODH                         PIC X.
            03 APRODO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 TEIGENH                        PIC X.
            03 TEIGENO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EEIGENH                        PIC X.
            03 EEIGENO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEIGENH                        PIC X.
            03 AEIGENO                        PIC X(45).
            03 FILLER                         PIC X(3).
            03 TBERPRH                        PIC X.
            03 TBERPRO                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 EBERPRH                        PIC X.
            03 EBERPRO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TANARTH                        PIC X.
            03 TANARTO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EANARTH                        PIC X.
            03 EANARTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TAUFNDH                        PIC X.
            03 TAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EAUFNDH                        PIC X.
            03 EAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TPESTRH                        PIC X.
            03 TPESTRO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EPESTRH                        PIC X.
            03 EPESTRO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TSTATIH                        PIC X.
            03 TSTATIO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESTATIH                        PIC X.
            03 ESTATIO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TSTATUH                        PIC X.
            03 TSTATUO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESTATUH                        PIC X.
            03 ESTATUO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ASTATUH                        PIC X.
            03 ASTATUO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).



