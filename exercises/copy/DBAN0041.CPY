      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0041.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DBAN0041.CPv  $
      *
      *   Rev 2.7   May 12 2014 13:50:12   t13ae23
      *AAN01165: AdvoCard AN03 Änderung Sepa/hand.Kennz. Feld ändern
      *
      *   Rev 2.6   Oct 24 2013 10:59:28   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.5   Jun 21 2013 13:22:28   t13ae34
      *AAN01073: Projekt SEPA - Kennzeichen Mandat
      *
      *   Rev 2.4   Mar 09 2007 16:52:04   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 19:31:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 18:31:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Feb 13 2007 17:24:18   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Dec 2006 12:02:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  INPUT DATA FOR MAP BAN0041
         01 BAN0041I.
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
            03 TVSNRL                         PIC S9(4) COMP.
            03 TVSNRF                         PIC X.
            03 FILLER REDEFINES TVSNRF.
               05 TVSNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVSNRI                         PIC X(7).
            03 AVSNRL                         PIC S9(4) COMP.
            03 AVSNRF                         PIC X.
            03 FILLER REDEFINES AVSNRF.
               05 AVSNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVSNRI                         PIC X(15).
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
            03 TPRODL                         PIC S9(4) COMP.
            03 TPRODF                         PIC X.
            03 FILLER REDEFINES TPRODF.
               05 TPRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI                         PIC X(10).
            03 EPRODL                         PIC S9(4) COMP.
            03 EPRODF                         PIC X.
            03 FILLER REDEFINES EPRODF.
               05 EPRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODI                         PIC X(2).
            03 TVEBEGL                        PIC S9(4) COMP.
            03 TVEBEGF                        PIC X.
            03 FILLER REDEFINES TVEBEGF.
               05 TVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVEBEGI                        PIC X(10).
            03 EVEBEGL                        PIC S9(4) COMP.
            03 EVEBEGF                        PIC X.
            03 FILLER REDEFINES EVEBEGF.
               05 EVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVEBEGI                        PIC X(6).
            03 TVEENDL                        PIC S9(4) COMP.
            03 TVEENDF                        PIC X.
            03 FILLER REDEFINES TVEENDF.
               05 TVEENDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVEENDI                        PIC X(10).
            03 EVEENDL                        PIC S9(4) COMP.
            03 EVEENDF                        PIC X.
            03 FILLER REDEFINES EVEENDF.
               05 EVEENDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVEENDI                        PIC X(6).
            03 TZAHLWL                        PIC S9(4) COMP.
            03 TZAHLWF                        PIC X.
            03 FILLER REDEFINES TZAHLWF.
               05 TZAHLWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TZAHLWI                        PIC X(10).
            03 EZAHLWL                        PIC S9(4) COMP.
            03 EZAHLWF                        PIC X.
            03 FILLER REDEFINES EZAHLWF.
               05 EZAHLWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EZAHLWI                        PIC X(1).
            03 TBEITRL                        PIC S9(4) COMP.
            03 TBEITRF                        PIC X.
            03 FILLER REDEFINES TBEITRF.
               05 TBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TBEITRI                        PIC X(10).
            03 EBEITRL                        PIC S9(4) COMP.
            03 EBEITRF                        PIC X.
            03 FILLER REDEFINES EBEITRF.
               05 EBEITRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBEITRI                        PIC X(9).
            03 TEINHL                         PIC S9(4) COMP.
            03 TEINHF                         PIC X.
            03 FILLER REDEFINES TEINHF.
               05 TEINHA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TEINHI                         PIC X(10).
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
            03 TKTNRL                         PIC S9(4) COMP.
            03 TKTNRF                         PIC X.
            03 FILLER REDEFINES TKTNRF.
               05 TKTNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TKTNRI                         PIC X(10).
            03 EKTONRL                        PIC S9(4) COMP.
            03 EKTONRF                        PIC X.
            03 FILLER REDEFINES EKTONRF.
               05 EKTONRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKTONRI                        PIC X(12).
            03 TBLZL                          PIC S9(4) COMP.
            03 TBLZF                          PIC X.
            03 FILLER REDEFINES TBLZF.
               05 TBLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TBLZI                          PIC X(3).
            03 EBLZL                          PIC S9(4) COMP.
            03 EBLZF                          PIC X.
            03 FILLER REDEFINES EBLZF.
               05 EBLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EBLZI                          PIC X(8).
            03 TAMTKZL                        PIC S9(4) COMP.
            03 TAMTKZF                        PIC X.
            03 FILLER REDEFINES TAMTKZF.
               05 TAMTKZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TAMTKZI                        PIC X(7).
            03 EAMTKZL                        PIC S9(4) COMP.
            03 EAMTKZF                        PIC X.
            03 FILLER REDEFINES EAMTKZF.
               05 EAMTKZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EAMTKZI                        PIC X(11).
            03 TIBANL                         PIC S9(4) COMP.
            03 TIBANF                         PIC X.
            03 FILLER REDEFINES TIBANF.
               05 TIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TIBANI                         PIC X(10).
            03 EIBANL                         PIC S9(4) COMP.
            03 EIBANF                         PIC X.
            03 FILLER REDEFINES EIBANF.
               05 EIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EIBANI                         PIC X(34).
            03 TBICL                          PIC S9(4) COMP.
            03 TBICF                          PIC X.
            03 FILLER REDEFINES TBICF.
               05 TBICA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TBICI                          PIC X(3).
            03 EBICL                          PIC S9(4) COMP.
            03 EBICF                          PIC X.
            03 FILLER REDEFINES EBICF.
               05 EBICA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EBICI                          PIC X(11).
            03 THANDJL                        PIC S9(4) COMP.
            03 THANDJF                        PIC X.
            03 FILLER REDEFINES THANDJF.
               05 THANDJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDJI                        PIC X(10).
            03 EHANDJL                        PIC S9(4) COMP.
            03 EHANDJF                        PIC X.
            03 FILLER REDEFINES EHANDJF.
               05 EHANDJA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDJI                        PIC X(1).
            03 TBERPRL                        PIC S9(4) COMP.
            03 TBERPRF                        PIC X.
            03 FILLER REDEFINES TBERPRF.
               05 TBERPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TBERPRI                        PIC X(14).
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
            03 TANARTI                        PIC X(10).
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

      *  OUTPUT DATA FOR MAP BAN0041
         01 BAN0041O REDEFINES BAN0041I.
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
            03 EPINH                          PIC X.
            03 EPINO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 EPINPRH                        PIC X.
            03 EPINPRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TVSNRH                         PIC X.
            03 TVSNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNRH                         PIC X.
            03 AVSNRO                         PIC X(15).
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
            03 TPRODH                         PIC X.
            03 TPRODO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPRODH                         PIC X.
            03 EPRODO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 TVEBEGH                        PIC X.
            03 TVEBEGO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EVEBEGH                        PIC X.
            03 EVEBEGO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TVEENDH                        PIC X.
            03 TVEENDO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EVEENDH                        PIC X.
            03 EVEENDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TZAHLWH                        PIC X.
            03 TZAHLWO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EZAHLWH                        PIC X.
            03 EZAHLWO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TBEITRH                        PIC X.
            03 TBEITRO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EBEITRH                        PIC X.
            03 EBEITRO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TEINHH                         PIC X.
            03 TEINHO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 AEINHH                         PIC X.
            03 AEINHO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 EEINHH                         PIC X.
            03 EEINHO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TKTNRH                         PIC X.
            03 TKTNRO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKTONRH                        PIC X.
            03 EKTONRO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 TBLZH                          PIC X.
            03 TBLZO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 EBLZH                          PIC X.
            03 EBLZO                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 TAMTKZH                        PIC X.
            03 TAMTKZO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EAMTKZH                        PIC X.
            03 EAMTKZO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 TIBANH                         PIC X.
            03 TIBANO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EIBANH                         PIC X.
            03 EIBANO                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 TBICH                          PIC X.
            03 TBICO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 EBICH                          PIC X.
            03 EBICO                          PIC X(11).
            03 FILLER                         PIC X(3).
            03 THANDJH                        PIC X.
            03 THANDJO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDJH                        PIC X.
            03 EHANDJO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TBERPRH                        PIC X.
            03 TBERPRO                        PIC X(14).
            03 FILLER                         PIC X(3).
            03 EBERPRH                        PIC X.
            03 EBERPRO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TANARTH                        PIC X.
            03 TANARTO                        PIC X(10).
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

