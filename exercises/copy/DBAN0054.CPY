      *  Input Data For Map BAN0054
      *  GEIGER (79) UND GEIGER-UMSATZSTEUER (80)
      *  PGAN0087.CBL
 
         01 BAN0054I.
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
            03 TVSNR2L                        PIC S9(4) COMP.
            03 TVSNR2F                        PIC X.
            03 FILLER REDEFINES TVSNR2F.
               05 TVSNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVSNR2I                        PIC X(8).
            03 EVSNR2L                        PIC S9(4) COMP.
            03 EVSNR2F                        PIC X.
            03 FILLER REDEFINES EVSNR2F.
               05 EVSNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVSNR2I                        PIC X(15).
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
            03 TANREDEL                       PIC S9(4) COMP.
            03 TANREDEF                       PIC X.
            03 FILLER REDEFINES TANREDEF.
               05 TANREDEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TANREDEI                       PIC X(10).
            03 EANREDEL                       PIC S9(4) COMP.
            03 EANREDEF                       PIC X.
            03 FILLER REDEFINES EANREDEF.
               05 EANREDEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EANREDEI                       PIC X(6).
            03 TKDTITL                        PIC S9(4) COMP.
            03 TKDTITF                        PIC X.
            03 FILLER REDEFINES TKDTITF.
               05 TKDTITA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDTITI                        PIC X(6).
            03 EKDTITL                        PIC S9(4) COMP.
            03 EKDTITF                        PIC X.
            03 FILLER REDEFINES EKDTITF.
               05 EKDTITA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDTITI                        PIC X(20).
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
            03 TGEBURL                        PIC S9(4) COMP.
            03 TGEBURF                        PIC X.
            03 FILLER REDEFINES TGEBURF.
               05 TGEBURA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TGEBURI                        PIC X(6).
            03 EGEBURL                        PIC S9(4) COMP.
            03 EGEBURF                        PIC X.
            03 FILLER REDEFINES EGEBURF.
               05 EGEBURA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGEBURI                        PIC X(6).
            03 TKDMAILL                       PIC S9(4) COMP.
            03 TKDMAILF                       PIC X.
            03 FILLER REDEFINES TKDMAILF.
               05 TKDMAILA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDMAILI                       PIC X(5).
            03 EKDMAILL                       PIC S9(4) COMP.
            03 EKDMAILF                       PIC X.
            03 FILLER REDEFINES EKDMAILF.
               05 EKDMAILA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDMAILI                       PIC X(50).
            03 TKDTELL                        PIC S9(4) COMP.
            03 TKDTELF                        PIC X.
            03 FILLER REDEFINES TKDTELF.
               05 TKDTELA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKDTELI                        PIC X(7).
            03 EKDTELL                        PIC S9(4) COMP.
            03 EKDTELF                        PIC X.
            03 FILLER REDEFINES EKDTELF.
               05 EKDTELA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKDTELI                        PIC X(30).
            03 TKDSTRAL                       PIC S9(4) COMP.
            03 TKDSTRAF                       PIC X.
            03 FILLER REDEFINES TKDSTRAF.
               05 TKDSTRAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKDSTRAI                       PIC X(10).
            03 EKDSTRAL                       PIC S9(4) COMP.
            03 EKDSTRAF                       PIC X.
            03 FILLER REDEFINES EKDSTRAF.
               05 EKDSTRAA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKDSTRAI                       PIC X(45).
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
            03 TKTOIZUL                       PIC S9(4) COMP.
            03 TKTOIZUF                       PIC X.
            03 FILLER REDEFINES TKTOIZUF.
               05 TKTOIZUA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKTOIZUI                       PIC X(10).
            03 EKTOIZNL                       PIC S9(4) COMP.
            03 EKTOIZNF                       PIC X.
            03 FILLER REDEFINES EKTOIZNF.
               05 EKTOIZNA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKTOIZNI                       PIC X(26).
            03 TKTOIVNL                       PIC S9(4) COMP.
            03 TKTOIVNF                       PIC X.
            03 FILLER REDEFINES TKTOIVNF.
               05 TKTOIVNA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKTOIVNI                       PIC X(11).
            03 EKTOIVNL                       PIC S9(4) COMP.
            03 EKTOIVNF                       PIC X.
            03 FILLER REDEFINES EKTOIVNF.
               05 EKTOIVNA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKTOIVNI                       PIC X(26).
            03 TVNIBANL                       PIC S9(4) COMP.
            03 TVNIBANF                       PIC X.
            03 FILLER REDEFINES TVNIBANF.
               05 TVNIBANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TVNIBANI                       PIC X(4).
            03 EVNIBANL                       PIC S9(4) COMP.
            03 EVNIBANF                       PIC X.
            03 FILLER REDEFINES EVNIBANF.
               05 EVNIBANA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EVNIBANI                       PIC X(34).
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
            03 TZWPL                          PIC S9(4) COMP.
            03 TZWPF                          PIC X.
            03 FILLER REDEFINES TZWPF.
               05 TZWPA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TZWPI                          PIC X(2).
            03 TANZBARL                       PIC S9(4) COMP.
            03 TANZBARF                       PIC X.
            03 FILLER REDEFINES TANZBARF.
               05 TANZBARA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TANZBARI                       PIC X(7).
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
            03 TRABL                          PIC S9(4) COMP.
            03 TRABF                          PIC X.
            03 FILLER REDEFINES TRABF.
               05 TRABA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TRABI                          PIC X(4).

            03 GRP0001I OCCURS 2.
 
               05 EPRODL                        PIC S9(4) COMP.
               05 EPRODF                        PIC X.
               05 FILLER REDEFINES EPRODF.
                  10 EPRODA                     PIC X.
               05 FILLER                        PIC X(1).
               05 EPRODI                        PIC X(3).
               05 ESUMMEL                       PIC S9(4) COMP.
               05 ESUMMEF                       PIC X.
               05 FILLER REDEFINES ESUMMEF.
                  10 ESUMMEA                    PIC X.
               05 FILLER                        PIC X(1).
               05 ESUMMEI                       PIC X(9).
               05 EZWPL                         PIC S9(4) COMP.
               05 EZWPF                         PIC X.
               05 FILLER REDEFINES EZWPF.
                  10 EZWPA                      PIC X.
               05 FILLER                        PIC X(1).
               05 EZWPI                         PIC X(1).
               05 EBARL                         PIC S9(4) COMP.
               05 EBARF                         PIC X.
               05 FILLER REDEFINES EBARF.
                  10 EBARA                      PIC X.
               05 FILLER                        PIC X(1).
               05 EBARI                         PIC X(4).
               05 ELAUFZL                       PIC S9(4) COMP.
               05 ELAUFZF                       PIC X.
               05 FILLER REDEFINES ELAUFZF.
                  10 ELAUFZA                    PIC X.
               05 FILLER                        PIC X(1).
               05 ELAUFZI                       PIC X(4).
               05 AEINHL                        PIC S9(4) COMP.
               05 AEINHF                        PIC X.
               05 FILLER REDEFINES AEINHF.
                  10 AEINHA                     PIC X.
               05 FILLER                        PIC X(1).
               05 AEINHI                        PIC X(7).
               05 EEINHL                        PIC S9(4) COMP.
               05 EEINHF                        PIC X.
               05 FILLER REDEFINES EEINHF.
                  10 EEINHA                     PIC X.
               05 FILLER                        PIC X(1).
               05 EEINHI                        PIC X(7).
               05 ERABL                         PIC S9(4) COMP.
               05 ERABF                         PIC X.
               05 FILLER REDEFINES ERABF.
                  10 ERABA                      PIC X.
               05 FILLER                        PIC X(1).
               05 ERABI                         PIC X(3).

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

      *  Output Data For Map BAN0054
         01 BAN0054O REDEFINES BAN0054I.
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
            03 TVSNR2H                        PIC X.
            03 TVSNR2O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 EVSNR2H                        PIC X.
            03 EVSNR2O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 TVSNRH                         PIC X.
            03 TVSNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVSNRH                         PIC X.
            03 AVSNRO                         PIC X(15).
            03 FILLER                         PIC X(3).
            03 TANREDEH                       PIC X.
            03 TANREDEO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EANREDEH                       PIC X.
            03 EANREDEO                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 TKDTITH                        PIC X.
            03 TKDTITO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EKDTITH                        PIC X.
            03 EKDTITO                        PIC X(20).
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
            03 TGEBURH                        PIC X.
            03 TGEBURO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EGEBURH                        PIC X.
            03 EGEBURO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TKDMAILH                       PIC X.
            03 TKDMAILO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 EKDMAILH                       PIC X.
            03 EKDMAILO                       PIC X(50).
            03 FILLER                         PIC X(3).
            03 TKDTELH                        PIC X.
            03 TKDTELO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EKDTELH                        PIC X.
            03 EKDTELO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 TKDSTRAH                       PIC X.
            03 TKDSTRAO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKDSTRAH                       PIC X.
            03 EKDSTRAO                       PIC X(45).
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
            03 TKTOIZUH                       PIC X.
            03 TKTOIZUO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKTOIZNH                       PIC X.
            03 EKTOIZNO                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 TKTOIVNH                       PIC X.
            03 TKTOIVNO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 EKTOIVNH                       PIC X.
            03 EKTOIVNO                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 TVNIBANH                       PIC X.
            03 TVNIBANO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 EVNIBANH                       PIC X.
            03 EVNIBANO                       PIC X(34).
            03 FILLER                         PIC X(3).
            03 TPRODH                         PIC X.
            03 TPRODO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 TSUMMEH                        PIC X.
            03 TSUMMEO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 TZWPH                          PIC X.
            03 TZWPO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 TANZBARH                       PIC X.
            03 TANZBARO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 TLAUFZH                        PIC X.
            03 TLAUFZO                        PIC X(12).
            03 FILLER                         PIC X(3).
            03 TEINHAH                        PIC X.
            03 TEINHAO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 TRABH                          PIC X.
            03 TRABO                          PIC X(4).
 
            03 GRP0001O OCCURS 2.
 
               05 FILLER                      PIC X(3).
               05 EPRODH                      PIC X.
               05 EPRODO                      PIC X(3).
               05 FILLER                      PIC X(3).
               05 ESUMMEH                     PIC X.
               05 ESUMMEO                     PIC X(9).
               05 FILLER                      PIC X(3).
               05 EZWPH                       PIC X.
               05 EZWPO                       PIC X(1).
               05 FILLER                      PIC X(3).
               05 EBARH                       PIC X.
               05 EBARO                       PIC X(4).
               05 FILLER                      PIC X(3).
               05 ELAUFZH                     PIC X.
               05 ELAUFZO                     PIC X(4).
               05 FILLER                      PIC X(3).
               05 AEINHH                      PIC X.
               05 AEINHO                      PIC X(7).
               05 FILLER                      PIC X(3).
               05 EEINHH                      PIC X.
               05 EEINHO                      PIC X(7).
               05 FILLER                      PIC X(3).
               05 ERABH                       PIC X.
               05 ERABO                       PIC X(3).
 
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



