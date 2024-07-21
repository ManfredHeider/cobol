      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-58)
      *   MAPSET NAME   BAN0040
      *   DATE CREATED  06/22/2009
      *   TIME CREATED  14:37:42

      *  INPUT DATA FOR MAP BAN0040
         01 BAN0040I.
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
            03 TPIN1L                         PIC S9(4) COMP.
            03 TPIN1F                         PIC X.
            03 FILLER REDEFINES TPIN1F.
               05 TPIN1A                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPIN1I                         PIC X(10).
            03 EPIN1L                         PIC S9(4) COMP.
            03 EPIN1F                         PIC X.
            03 FILLER REDEFINES EPIN1F.
               05 EPIN1A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPIN1I                         PIC X(7).
            03 EPINPR1L                       PIC S9(4) COMP.
            03 EPINPR1F                       PIC X.
            03 FILLER REDEFINES EPINPR1F.
               05 EPINPR1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPINPR1I                       PIC X(2).
            03 TPIN2L                         PIC S9(4) COMP.
            03 TPIN2F                         PIC X.
            03 FILLER REDEFINES TPIN2F.
               05 TPIN2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPIN2I                         PIC X(10).
            03 EPIN2L                         PIC S9(4) COMP.
            03 EPIN2F                         PIC X.
            03 FILLER REDEFINES EPIN2F.
               05 EPIN2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPIN2I                         PIC X(7).
            03 EPINPR2L                       PIC S9(4) COMP.
            03 EPINPR2F                       PIC X.
            03 FILLER REDEFINES EPINPR2F.
               05 EPINPR2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPINPR2I                       PIC X(2).
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
            03 TPROD1L                        PIC S9(4) COMP.
            03 TPROD1F                        PIC X.
            03 FILLER REDEFINES TPROD1F.
               05 TPROD1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPROD1I                        PIC X(10).
            03 EPROD1L                        PIC S9(4) COMP.
            03 EPROD1F                        PIC X.
            03 FILLER REDEFINES EPROD1F.
               05 EPROD1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPROD1I                        PIC X(2).
            03 TSUMME1L                       PIC S9(4) COMP.
            03 TSUMME1F                       PIC X.
            03 FILLER REDEFINES TSUMME1F.
               05 TSUMME1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TSUMME1I                       PIC X(6).
            03 ESUMME1L                       PIC S9(4) COMP.
            03 ESUMME1F                       PIC X.
            03 FILLER REDEFINES ESUMME1F.
               05 ESUMME1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESUMME1I                       PIC X(9).
            03 TEINH1L                        PIC S9(4) COMP.
            03 TEINH1F                        PIC X.
            03 FILLER REDEFINES TEINH1F.
               05 TEINH1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TEINH1I                        PIC X(10).
            03 AEINH1L                        PIC S9(4) COMP.
            03 AEINH1F                        PIC X.
            03 FILLER REDEFINES AEINH1F.
               05 AEINH1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AEINH1I                        PIC X(7).
            03 EEINH1L                        PIC S9(4) COMP.
            03 EEINH1F                        PIC X.
            03 FILLER REDEFINES EEINH1F.
               05 EEINH1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEINH1I                        PIC X(7).
            03 TPROD2L                        PIC S9(4) COMP.
            03 TPROD2F                        PIC X.
            03 FILLER REDEFINES TPROD2F.
               05 TPROD2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPROD2I                        PIC X(10).
            03 EPROD2L                        PIC S9(4) COMP.
            03 EPROD2F                        PIC X.
            03 FILLER REDEFINES EPROD2F.
               05 EPROD2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPROD2I                        PIC X(2).
            03 TSUMME2L                       PIC S9(4) COMP.
            03 TSUMME2F                       PIC X.
            03 FILLER REDEFINES TSUMME2F.
               05 TSUMME2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TSUMME2I                       PIC X(6).
            03 ESUMME2L                       PIC S9(4) COMP.
            03 ESUMME2F                       PIC X.
            03 FILLER REDEFINES ESUMME2F.
               05 ESUMME2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESUMME2I                       PIC X(9).
            03 TEINH2L                        PIC S9(4) COMP.
            03 TEINH2F                        PIC X.
            03 FILLER REDEFINES TEINH2F.
               05 TEINH2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TEINH2I                        PIC X(10).
            03 AEINH2L                        PIC S9(4) COMP.
            03 AEINH2F                        PIC X.
            03 FILLER REDEFINES AEINH2F.
               05 AEINH2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AEINH2I                        PIC X(7).
            03 EEINH2L                        PIC S9(4) COMP.
            03 EEINH2F                        PIC X.
            03 FILLER REDEFINES EEINH2F.
               05 EEINH2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEINH2I                        PIC X(7).
            03 THANDBL                        PIC S9(4) COMP.
            03 THANDBF                        PIC X.
            03 FILLER REDEFINES THANDBF.
               05 THANDBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDBI                        PIC X(10).
            03 EHANDBL                        PIC S9(4) COMP.
            03 EHANDBF                        PIC X.
            03 FILLER REDEFINES EHANDBF.
               05 EHANDBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDBI                        PIC X(1).
            03 THANDTL                        PIC S9(4) COMP.
            03 THANDTF                        PIC X.
            03 FILLER REDEFINES THANDTF.
               05 THANDTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDTI                        PIC X(10).
            03 EHANDTL                        PIC S9(4) COMP.
            03 EHANDTF                        PIC X.
            03 FILLER REDEFINES EHANDTF.
               05 EHANDTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDTI                        PIC X(1).
            03 THANDFL                        PIC S9(4) COMP.
            03 THANDFF                        PIC X.
            03 FILLER REDEFINES THANDFF.
               05 THANDFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDFI                        PIC X(10).
            03 EHANDFL                        PIC S9(4) COMP.
            03 EHANDFF                        PIC X.
            03 FILLER REDEFINES EHANDFF.
               05 EHANDFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDFI                        PIC X(1).
            03 THANDLL                        PIC S9(4) COMP.
            03 THANDLF                        PIC X.
            03 FILLER REDEFINES THANDLF.
               05 THANDLA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDLI                        PIC X(10).
            03 EHANDLL                        PIC S9(4) COMP.
            03 EHANDLF                        PIC X.
            03 FILLER REDEFINES EHANDLF.
               05 EHANDLA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDLI                        PIC X(1).
            03 THANDVL                        PIC S9(4) COMP.
            03 THANDVF                        PIC X.
            03 FILLER REDEFINES THANDVF.
               05 THANDVA                        PIC X.
            03 FILLER                         PIC X(1).
            03 THANDVI                        PIC X(10).
            03 EHANDVL                        PIC S9(4) COMP.
            03 EHANDVF                        PIC X.
            03 FILLER REDEFINES EHANDVF.
               05 EHANDVA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDVI                        PIC X(1).
            03 THAND2L                        PIC S9(4) COMP.
            03 THAND2F                        PIC X.
            03 FILLER REDEFINES THAND2F.
               05 THAND2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 THAND2I                        PIC X(10).
            03 EHAND2L                        PIC S9(4) COMP.
            03 EHAND2F                        PIC X.
            03 FILLER REDEFINES EHAND2F.
               05 EHAND2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EHAND2I                        PIC X(1).
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

      *  OUTPUT DATA FOR MAP BAN0040
         01 BAN0040O REDEFINES BAN0040I.
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
            03 TPIN1H                         PIC X.
            03 TPIN1O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPIN1H                         PIC X.
            03 EPIN1O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 EPINPR1H                       PIC X.
            03 EPINPR1O                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 TPIN2H                         PIC X.
            03 TPIN2O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPIN2H                         PIC X.
            03 EPIN2O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 EPINPR2H                       PIC X.
            03 EPINPR2O                       PIC X(2).
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
            03 TPROD1H                        PIC X.
            03 TPROD1O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPROD1H                        PIC X.
            03 EPROD1O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TSUMME1H                       PIC X.
            03 TSUMME1O                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESUMME1H                       PIC X.
            03 ESUMME1O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 TEINH1H                        PIC X.
            03 TEINH1O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AEINH1H                        PIC X.
            03 AEINH1O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EEINH1H                        PIC X.
            03 EEINH1O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPROD2H                        PIC X.
            03 TPROD2O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPROD2H                        PIC X.
            03 EPROD2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TSUMME2H                       PIC X.
            03 TSUMME2O                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESUMME2H                       PIC X.
            03 ESUMME2O                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 TEINH2H                        PIC X.
            03 TEINH2O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AEINH2H                        PIC X.
            03 AEINH2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EEINH2H                        PIC X.
            03 EEINH2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 THANDBH                        PIC X.
            03 THANDBO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDBH                        PIC X.
            03 EHANDBO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THANDTH                        PIC X.
            03 THANDTO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDTH                        PIC X.
            03 EHANDTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THANDFH                        PIC X.
            03 THANDFO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDFH                        PIC X.
            03 EHANDFO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THANDLH                        PIC X.
            03 THANDLO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDLH                        PIC X.
            03 EHANDLO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THANDVH                        PIC X.
            03 THANDVO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHANDVH                        PIC X.
            03 EHANDVO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THAND2H                        PIC X.
            03 THAND2O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EHAND2H                        PIC X.
            03 EHAND2O                        PIC X(1).
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

