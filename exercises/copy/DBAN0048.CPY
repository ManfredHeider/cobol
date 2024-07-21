      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0048.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DBAN0048.CPv  $
      *
      *   Rev 2.3   Feb 17 2021 12:15:24   t13ae23
      *ANTRAG-860 - Vermittlerprovision -
      *
      *   Rev 2.2   Dec 14 2011 17:47:04   t13ae23
      *AAN01043: Erweiterung der Antragserfassungsdialoge
      *
      *   Rev 2.1   03 Jan 2007 18:54:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Dec 2006 14:13:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.010.0034
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-52)
      *   MAPSET NAME   BAN0048
      *   DATE CREATED  17/02/2021
      *   TIME CREATED  10:00:55

      *  INPUT DATA FOR MAP BAN0048
         01 BAN0048I.
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
            03 TVNRL                          PIC S9(4) COMP.
            03 TVNRF                          PIC X.
            03 FILLER REDEFINES TVNRF.
               05 TVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TVNRI                          PIC X(10).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER REDEFINES EVNRF.
               05 EVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 TKDNAML                        PIC S9(4) COMP.
            03 TKDNAMF                        PIC X.
            03 FILLER REDEFINES TKDNAMF.
               05 TKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDNAMI                        PIC X(6).
            03 EKDNAML                        PIC S9(4) COMP.
            03 EKDNAMF                        PIC X.
            03 FILLER REDEFINES EKDNAMF.
               05 EKDNAMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDNAMI                        PIC X(23).
            03 TKDVNAL                        PIC S9(4) COMP.
            03 TKDVNAF                        PIC X.
            03 FILLER REDEFINES TKDVNAF.
               05 TKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDVNAI                        PIC X(7).
            03 EKDVNAL                        PIC S9(4) COMP.
            03 EKDVNAF                        PIC X.
            03 FILLER REDEFINES EKDVNAF.
               05 EKDVNAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDVNAI                        PIC X(23).
            03 TKDSTAAL                       PIC S9(4) COMP.
            03 TKDSTAAF                       PIC X.
            03 FILLER REDEFINES TKDSTAAF.
               05 TKDSTAAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDSTAAI                       PIC X(7).
            03 EKDSTAAL                       PIC S9(4) COMP.
            03 EKDSTAAF                       PIC X.
            03 FILLER REDEFINES EKDSTAAF.
               05 EKDSTAAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDSTAAI                       PIC X(3).
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
            03 TKDGEBL                        PIC S9(4) COMP.
            03 TKDGEBF                        PIC X.
            03 FILLER REDEFINES TKDGEBF.
               05 TKDGEBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDGEBI                        PIC X(6).
            03 EKDGEBL                        PIC S9(4) COMP.
            03 EKDGEBF                        PIC X.
            03 FILLER REDEFINES EKDGEBF.
               05 EKDGEBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDGEBI                        PIC X(6).
            03 TKDPEPL                        PIC S9(4) COMP.
            03 TKDPEPF                        PIC X.
            03 FILLER REDEFINES TKDPEPF.
               05 TKDPEPA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDPEPI                        PIC X(6).
            03 EKDPEPL                        PIC S9(4) COMP.
            03 EKDPEPF                        PIC X.
            03 FILLER REDEFINES EKDPEPF.
               05 EKDPEPA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDPEPI                        PIC X(1).
            03 TKDBGRPL                       PIC S9(4) COMP.
            03 TKDBGRPF                       PIC X.
            03 FILLER REDEFINES TKDBGRPF.
               05 TKDBGRPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDBGRPI                       PIC X(6).
            03 EKDBGRPL                       PIC S9(4) COMP.
            03 EKDBGRPF                       PIC X.
            03 FILLER REDEFINES EKDBGRPF.
               05 EKDBGRPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDBGRPI                       PIC X(30).
            03 TKDBRANL                       PIC S9(4) COMP.
            03 TKDBRANF                       PIC X.
            03 FILLER REDEFINES TKDBRANF.
               05 TKDBRANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDBRANI                       PIC X(7).
            03 EKDBRANL                       PIC S9(4) COMP.
            03 EKDBRANF                       PIC X.
            03 FILLER REDEFINES EKDBRANF.
               05 EKDBRANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDBRANI                       PIC X(30).
            03 TPRODL                         PIC S9(4) COMP.
            03 TPRODF                         PIC X.
            03 FILLER REDEFINES TPRODF.
               05 TPRODA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI                         PIC X(4).
            03 TSUMMEL                        PIC S9(4) COMP.
            03 TSUMMEF                        PIC X.
            03 FILLER REDEFINES TSUMMEF.
               05 TSUMMEA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TSUMMEI                        PIC X(9).
            03 TVERMPL                        PIC S9(4) COMP.
            03 TVERMPF                        PIC X.
            03 FILLER REDEFINES TVERMPF.
               05 TVERMPA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVERMPI                        PIC X(9).
            03 TZWPL                          PIC S9(4) COMP.
            03 TZWPF                          PIC X.
            03 FILLER REDEFINES TZWPF.
               05 TZWPA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TZWPI                          PIC X(2).
            03 TLAUFZL                        PIC S9(4) COMP.
            03 TLAUFZF                        PIC X.
            03 FILLER REDEFINES TLAUFZF.
               05 TLAUFZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TLAUFZI                        PIC X(12).
            03 TEINHAL                        PIC S9(4) COMP.
            03 TEINHAF                        PIC X.
            03 FILLER REDEFINES TEINHAF.
               05 TEINHAA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TEINHAI                        PIC X(10).

            03 GRP0001I OCCURS 8.

               05 EPRODL                        PIC S9(4) COMP.
               05 EPRODF                        PIC X.
               05 FILLER REDEFINES EPRODF.
                  07 EPRODA                     PIC X.
               05 FILLER                        PIC X(1).
               05 EPRODI                        PIC X(2).

               05 ESUMMEL                       PIC S9(4) COMP.
               05 ESUMMEF                       PIC X.
               05 FILLER REDEFINES ESUMMEF.
                  07 ESUMMEA                    PIC X.
               05 FILLER                        PIC X(1).
               05 ESUMMEI                       PIC X(9).

               05 EVERMPL                       PIC S9(4) COMP.
               05 EVERMPF                       PIC X.
               05 FILLER REDEFINES EVERMPF.
                  07 EVERMPA                    PIC X.
               05 FILLER                        PIC X(1).
               05 EVERMPI                       PIC X(9).

               05 EZWPL                         PIC S9(4) COMP.
               05 EZWPF                         PIC X.
               05 FILLER REDEFINES EZWPF.
                  07 EZWPA                      PIC X.
               05 FILLER                        PIC X(1).
               05 EZWPI                         PIC X(1).

               05 ELAUFZL                       PIC S9(4) COMP.
               05 ELAUFZF                       PIC X.
               05 FILLER REDEFINES ELAUFZF.
                  07 ELAUFZA                    PIC X.
               05 FILLER                        PIC X(1).
               05 ELAUFZI                       PIC X(4).

               05 AEINHL                        PIC S9(4) COMP.
               05 AEINHF                        PIC X.
               05 FILLER REDEFINES AEINHF.
                  07 AEINHA                     PIC X.
               05 FILLER                        PIC X(1).
               05 AEINHI                        PIC X(7).

               05 EEINHL                      PIC S9(4) COMP.
               05 EEINHF                      PIC X.
               05 FILLER REDEFINES EEINHF.
                  07 EEINHA                   PIC X.
               05 FILLER                      PIC X(1).
               05 EEINHI                      PIC X(7).

            03 TANARTL                        PIC S9(4) COMP.
            03 TANARTF                        PIC X.
            03 FILLER REDEFINES TANARTF.
               05 TANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TANARTI                        PIC X(3).
            03 EANARTL                        PIC S9(4) COMP.
            03 EANARTF                        PIC X.
            03 FILLER REDEFINES EANARTF.
               05 EANARTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EANARTI                        PIC X(1).
            03 TDYNPRL                        PIC S9(4) COMP.
            03 TDYNPRF                        PIC X.
            03 FILLER REDEFINES TDYNPRF.
               05 TDYNPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TDYNPRI                        PIC X(5).
            03 EDYNPRL                        PIC S9(4) COMP.
            03 EDYNPRF                        PIC X.
            03 FILLER REDEFINES EDYNPRF.
               05 EDYNPRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EDYNPRI                        PIC X(7).
            03 TAUFNDL                        PIC S9(4) COMP.
            03 TAUFNDF                        PIC X.
            03 FILLER REDEFINES TAUFNDF.
               05 TAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TAUFNDI                        PIC X(4).
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
            03 TPESTRI                        PIC X(4).
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
            03 TSTATUI                        PIC X(3).
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
            03 ASTATUI                        PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0048
         01 BAN0048O REDEFINES BAN0048I.
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
            03 TVNRH                          PIC X.
            03 TVNRO                          PIC X(10).
            03 FILLER                         PIC X(3).
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(3).
            03 TKDNAMH                        PIC X.
            03 TKDNAMO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EKDNAMH                        PIC X.
            03 EKDNAMO                        PIC X(23).
            03 FILLER                         PIC X(3).
            03 TKDVNAH                        PIC X.
            03 TKDVNAO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDVNAH                        PIC X.
            03 EKDVNAO                        PIC X(23).
            03 FILLER                         PIC X(3).
            03 TKDSTAAH                       PIC X.
            03 TKDSTAAO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDSTAAH                       PIC X.
            03 EKDSTAAO                       PIC X(3).
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
            03 TKDGEBH                        PIC X.
            03 TKDGEBO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EKDGEBH                        PIC X.
            03 EKDGEBO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TKDPEPH                        PIC X.
            03 TKDPEPO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EKDPEPH                        PIC X.
            03 EKDPEPO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TKDBGRPH                       PIC X.
            03 TKDBGRPO                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 EKDBGRPH                       PIC X.
            03 EKDBGRPO                       PIC X(30).
            03 FILLER                         PIC X(3).
            03 TKDBRANH                       PIC X.
            03 TKDBRANO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDBRANH                       PIC X.
            03 EKDBRANO                       PIC X(30).
            03 FILLER                         PIC X(3).
            03 TPRODH                         PIC X.
            03 TPRODO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 TSUMMEH                        PIC X.
            03 TSUMMEO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TVERMPH                        PIC X.
            03 TVERMPO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TZWPH                          PIC X.
            03 TZWPO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 TLAUFZH                        PIC X.
            03 TLAUFZO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 TEINHAH                        PIC X.
            03 TEINHAO                        PIC X(10).

            03 GRP0001O OCCURS 8.

               05 FILLER                      PIC X(3).
               05 EPRODH                      PIC X.
               05 EPRODO                      PIC X(2).

               05 FILLER                      PIC X(3).
               05 ESUMMEH                     PIC X.
               05 ESUMMEO                     PIC X(9).

               05 FILLER                      PIC X(3).
               05 EVERMPH                     PIC X.
               05 EVERMPO                     PIC X(9).

               05 FILLER                      PIC X(3).
               05 EZWPH                       PIC X.
               05 EZWPO                       PIC X(1).

               05 FILLER                      PIC X(3).
               05 ELAUFZH                     PIC X.
               05 ELAUFZO                     PIC X(4).

               05 FILLER                      PIC X(3).
               05 AEINHH                      PIC X.
               05 AEINHO                      PIC X(7).

               05 FILLER                      PIC X(3).
               05 EEINHH                      PIC X.
               05 EEINHO                      PIC X(7).

            03 FILLER                         PIC X(3).
            03 TANARTH                        PIC X.
            03 TANARTO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 EANARTH                        PIC X.
            03 EANARTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TDYNPRH                        PIC X.
            03 TDYNPRO                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EDYNPRH                        PIC X.
            03 EDYNPRO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 TAUFNDH                        PIC X.
            03 TAUFNDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 EAUFNDH                        PIC X.
            03 EAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TPESTRH                        PIC X.
            03 TPESTRO                        PIC X(4).
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
            03 TSTATUO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 ESTATUH                        PIC X.
            03 ESTATUO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ASTATUH                        PIC X.
            03 ASTATUO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

