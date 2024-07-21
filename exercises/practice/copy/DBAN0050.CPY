      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0050.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DBAN0050.CPv  $
      *
      *   Rev 2.10   Nov 09 2015 12:38:16   t13ae23
      *AAN01234: Ges.90 Prüfung Formblatt als Ersatz für Zertifikat
      *
      *   Rev 2.9   Aug 12 2015 14:49:04   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.8   Jul 07 2015 15:54:34   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.7   Jun 22 2015 15:44:46   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.6   Jun 10 2015 17:38:52   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.5   Jun 10 2015 14:49:50   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.4   Jun 10 2015 14:47:56   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 14:46:20   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:23:42   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:06:50   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Apr 22 2015 15:33:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.013.0041
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-57)
      *   MAPSET NAME   BAN0050
      *   DATE CREATED  11/03/2015
      *   TIME CREATED  11:30:42

      *  INPUT DATA FOR MAP BAN0050
         01 BAN0050I.
            03 FILLER                         PIC X(12).
            03 TVBNR2L                        PIC S9(4) COMP.
            03 TVBNR2F                        PIC X.
            03 FILLER REDEFINES TVBNR2F.
               05 TVBNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNR2I                        PIC X(2).
            03 AVBNR2L                        PIC S9(4) COMP.
            03 AVBNR2F                        PIC X.
            03 FILLER REDEFINES AVBNR2F.
               05 AVBNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNR2I                        PIC X(7).
            03 AVBPRZ2L                       PIC S9(4) COMP.
            03 AVBPRZ2F                       PIC X.
            03 FILLER REDEFINES AVBPRZ2F.
               05 AVBPRZ2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPRZ2I                       PIC X(1).
            03 ADIR2L                         PIC S9(4) COMP.
            03 ADIR2F                         PIC X.
            03 FILLER REDEFINES ADIR2F.
               05 ADIR2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 ADIR2I                         PIC X(3).
            03 AVBNAM2L                       PIC S9(4) COMP.
            03 AVBNAM2F                       PIC X.
            03 FILLER REDEFINES AVBNAM2F.
               05 AVBNAM2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAM2I                       PIC X(20).
            03 TVMNR2L                        PIC S9(4) COMP.
            03 TVMNR2F                        PIC X.
            03 FILLER REDEFINES TVMNR2F.
               05 TVMNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVMNR2I                        PIC X(2).
            03 AVMNR2L                        PIC S9(4) COMP.
            03 AVMNR2F                        PIC X.
            03 FILLER REDEFINES AVMNR2F.
               05 AVMNR2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNR2I                        PIC X(7).
            03 AVMPRZ2L                       PIC S9(4) COMP.
            03 AVMPRZ2F                       PIC X.
            03 FILLER REDEFINES AVMPRZ2F.
               05 AVMPRZ2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVMPRZ2I                       PIC X(1).
            03 AVMNAM2L                       PIC S9(4) COMP.
            03 AVMNAM2F                       PIC X.
            03 FILLER REDEFINES AVMNAM2F.
               05 AVMNAM2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNAM2I                       PIC X(20).
            03 TVNR2L                         PIC S9(4) COMP.
            03 TVNR2F                         PIC X.
            03 FILLER REDEFINES TVNR2F.
               05 TVNR2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVNR2I                         PIC X(11).
            03 EVNR2L                         PIC S9(4) COMP.
            03 EVNR2F                         PIC X.
            03 FILLER REDEFINES EVNR2F.
               05 EVNR2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVNR2I                         PIC X(15).
            03 TVORSC2L                       PIC S9(4) COMP.
            03 TVORSC2F                       PIC X.
            03 FILLER REDEFINES TVORSC2F.
               05 TVORSC2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TVORSC2I                       PIC X(7).
            03 AVORSC2L                       PIC S9(4) COMP.
            03 AVORSC2F                       PIC X.
            03 FILLER REDEFINES AVORSC2F.
               05 AVORSC2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVORSC2I                       PIC X(17).
            03 TVEBEGL                        PIC S9(4) COMP.
            03 TVEBEGF                        PIC X.
            03 FILLER REDEFINES TVEBEGF.
               05 TVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVEBEGI                        PIC X(7).
            03 EVEBEGL                        PIC S9(4) COMP.
            03 EVEBEGF                        PIC X.
            03 FILLER REDEFINES EVEBEGF.
               05 EVEBEGA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVEBEGI                        PIC X(6).
            03 TEGEBUEL                       PIC S9(4) COMP.
            03 TEGEBUEF                       PIC X.
            03 FILLER REDEFINES TEGEBUEF.
               05 TEGEBUEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TEGEBUEI                       PIC X(12).
            03 EEGEBUEL                       PIC S9(4) COMP.
            03 EEGEBUEF                       PIC X.
            03 FILLER REDEFINES EEGEBUEF.
               05 EEGEBUEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EEGEBUEI                       PIC X(1).
            03 TPRODIL                        PIC S9(4) COMP.
            03 TPRODIF                        PIC X.
            03 FILLER REDEFINES TPRODIF.
               05 TPRODIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODII                        PIC X(6).
            03 EPROD1L                        PIC S9(4) COMP.
            03 EPROD1F                        PIC X.
            03 FILLER REDEFINES EPROD1F.
               05 EPROD1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPROD1I                        PIC X(2).
            03 TPRODI4L                       PIC S9(4) COMP.
            03 TPRODI4F                       PIC X.
            03 FILLER REDEFINES TPRODI4F.
               05 TPRODI4A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI4I                       PIC X(4).
            03 AEINH1L                        PIC S9(4) COMP.
            03 AEINH1F                        PIC X.
            03 FILLER REDEFINES AEINH1F.
               05 AEINH1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AEINH1I                        PIC X(7).
            03 TPRODI2L                       PIC S9(4) COMP.
            03 TPRODI2F                       PIC X.
            03 FILLER REDEFINES TPRODI2F.
               05 TPRODI2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI2I                       PIC X(6).
            03 EPROD2L                        PIC S9(4) COMP.
            03 EPROD2F                        PIC X.
            03 FILLER REDEFINES EPROD2F.
               05 EPROD2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPROD2I                        PIC X(2).
            03 TPRODI3L                       PIC S9(4) COMP.
            03 TPRODI3F                       PIC X.
            03 FILLER REDEFINES TPRODI3F.
               05 TPRODI3A                       PIC X.
            03 FILLER                         PIC X(1).
            03 TPRODI3I                       PIC X(4).
            03 AEINH2L                        PIC S9(4) COMP.
            03 AEINH2F                        PIC X.
            03 FILLER REDEFINES AEINH2F.
               05 AEINH2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 AEINH2I                        PIC X(7).
            03 TINHL                          PIC S9(4) COMP.
            03 TINHF                          PIC X.
            03 FILLER REDEFINES TINHF.
               05 TINHA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TINHI                          PIC X(7).
            03 EINH1L                         PIC S9(4) COMP.
            03 EINH1F                         PIC X.
            03 FILLER REDEFINES EINH1F.
               05 EINH1A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EINH1I                         PIC X(70).
            03 EINH2L                         PIC S9(4) COMP.
            03 EINH2F                         PIC X.
            03 FILLER REDEFINES EINH2F.
               05 EINH2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EINH2I                         PIC X(30).
            03 TKIHVNL                        PIC S9(4) COMP.
            03 TKIHVNF                        PIC X.
            03 FILLER REDEFINES TKIHVNF.
               05 TKIHVNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKIHVNI                        PIC X(25).
            03 EKIHVNL                        PIC S9(4) COMP.
            03 EKIHVNF                        PIC X.
            03 FILLER REDEFINES EKIHVNF.
               05 EKIHVNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKIHVNI                        PIC X(30).
            03 TKIHZNL                        PIC S9(4) COMP.
            03 TKIHZNF                        PIC X.
            03 FILLER REDEFINES TKIHZNF.
               05 TKIHZNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKIHZNI                        PIC X(10).
            03 EKIHZNL                        PIC S9(4) COMP.
            03 EKIHZNF                        PIC X.
            03 FILLER REDEFINES EKIHZNF.
               05 EKIHZNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKIHZNI                        PIC X(30).
            03 TKIHSTRL                       PIC S9(4) COMP.
            03 TKIHSTRF                       PIC X.
            03 FILLER REDEFINES TKIHSTRF.
               05 TKIHSTRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKIHSTRI                       PIC X(11).
            03 EKIHSTRL                       PIC S9(4) COMP.
            03 EKIHSTRF                       PIC X.
            03 FILLER REDEFINES EKIHSTRF.
               05 EKIHSTRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKIHSTRI                       PIC X(45).
            03 TKIHPLZL                       PIC S9(4) COMP.
            03 TKIHPLZF                       PIC X.
            03 FILLER REDEFINES TKIHPLZF.
               05 TKIHPLZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKIHPLZI                       PIC X(8).
            03 EKIHPLZL                       PIC S9(4) COMP.
            03 EKIHPLZF                       PIC X.
            03 FILLER REDEFINES EKIHPLZF.
               05 EKIHPLZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKIHPLZI                       PIC X(5).
            03 TKIHORTL                       PIC S9(4) COMP.
            03 TKIHORTF                       PIC X.
            03 FILLER REDEFINES TKIHORTF.
               05 TKIHORTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TKIHORTI                       PIC X(8).
            03 EKIHORTL                       PIC S9(4) COMP.
            03 EKIHORTF                       PIC X.
            03 FILLER REDEFINES EKIHORTF.
               05 EKIHORTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EKIHORTI                       PIC X(50).
            03 TGELDIL                        PIC S9(4) COMP.
            03 TGELDIF                        PIC X.
            03 FILLER REDEFINES TGELDIF.
               05 TGELDIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TGELDII                        PIC X(13).
            03 EGELDIL                        PIC S9(4) COMP.
            03 EGELDIF                        PIC X.
            03 FILLER REDEFINES EGELDIF.
               05 EGELDIA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGELDII                        PIC X(58).
            03 TBICL                          PIC S9(4) COMP.
            03 TBICF                          PIC X.
            03 FILLER REDEFINES TBICF.
               05 TBICA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TBICI                          PIC X(4).
            03 EBICL                          PIC S9(4) COMP.
            03 EBICF                          PIC X.
            03 FILLER REDEFINES EBICF.
               05 EBICA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EBICI                          PIC X(11).
            03 TIBANL                         PIC S9(4) COMP.
            03 TIBANF                         PIC X.
            03 FILLER REDEFINES TIBANF.
               05 TIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TIBANI                         PIC X(5).
            03 EIBANL                         PIC S9(4) COMP.
            03 EIBANF                         PIC X.
            03 FILLER REDEFINES EIBANF.
               05 EIBANA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EIBANI                         PIC X(34).
            03 TSEPA-ML                       PIC S9(4) COMP.
            03 TSEPA-MF                       PIC X.
            03 FILLER REDEFINES TSEPA-MF.
               05 TSEPA-MA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TSEPA-MI                       PIC X(19).
            03 ESEPA-ML                       PIC S9(4) COMP.
            03 ESEPA-MF                       PIC X.
            03 FILLER REDEFINES ESEPA-MF.
               05 ESEPA-MA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEPA-MI                       PIC X(6).
            03 TFACOPTL                       PIC S9(4) COMP.
            03 TFACOPTF                       PIC X.
            03 FILLER REDEFINES TFACOPTF.
               05 TFACOPTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TFACOPTI                       PIC X(14).
            03 EFACOPTL                       PIC S9(4) COMP.
            03 EFACOPTF                       PIC X.
            03 FILLER REDEFINES EFACOPTF.
               05 EFACOPTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFACOPTI                       PIC X(1).
            03 TUMSATZL                       PIC S9(4) COMP.
            03 TUMSATZF                       PIC X.
            03 FILLER REDEFINES TUMSATZF.
               05 TUMSATZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TUMSATZI                       PIC X(7).
            03 EUMSATZL                       PIC S9(4) COMP.
            03 EUMSATZF                       PIC X.
            03 FILLER REDEFINES EUMSATZF.
               05 EUMSATZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EUMSATZI                       PIC X(9).
            03 TGELDWL                        PIC S9(4) COMP.
            03 TGELDWF                        PIC X.
            03 FILLER REDEFINES TGELDWF.
               05 TGELDWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TGELDWI                        PIC X(16).
            03 EGELDWL                        PIC S9(4) COMP.
            03 EGELDWF                        PIC X.
            03 FILLER REDEFINES EGELDWF.
               05 EGELDWA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGELDWI                        PIC X(1).
            03 TGARANTL                       PIC S9(4) COMP.
            03 TGARANTF                       PIC X.
            03 FILLER REDEFINES TGARANTF.
               05 TGARANTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TGARANTI                       PIC X(19).
            03 EGARANTL                       PIC S9(4) COMP.
            03 EGARANTF                       PIC X.
            03 FILLER REDEFINES EGARANTF.
               05 EGARANTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EGARANTI                       PIC X(1).
            03 TNEGATL                        PIC S9(4) COMP.
            03 TNEGATF                        PIC X.
            03 FILLER REDEFINES TNEGATF.
               05 TNEGATA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TNEGATI                        PIC X(18).
            03 ENEGATL                        PIC S9(4) COMP.
            03 ENEGATF                        PIC X.
            03 FILLER REDEFINES ENEGATF.
               05 ENEGATA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ENEGATI                        PIC X(1).
            03 THANDL                         PIC S9(4) COMP.
            03 THANDF                         PIC X.
            03 FILLER REDEFINES THANDF.
               05 THANDA                         PIC X.
            03 FILLER                         PIC X(1).
            03 THANDI                         PIC X(23).
            03 EHANDL                         PIC S9(4) COMP.
            03 EHANDF                         PIC X.
            03 FILLER REDEFINES EHANDF.
               05 EHANDA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EHANDI                         PIC X(1).
            03 TFORMBL                        PIC S9(4) COMP.
            03 TFORMBF                        PIC X.
            03 FILLER REDEFINES TFORMBF.
               05 TFORMBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TFORMBI                        PIC X(30).
            03 EFORMBL                        PIC S9(4) COMP.
            03 EFORMBF                        PIC X.
            03 FILLER REDEFINES EFORMBF.
               05 EFORMBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFORMBI                        PIC X(1).
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
            03 TAUFNDL                        PIC S9(4) COMP.
            03 TAUFNDF                        PIC X.
            03 FILLER REDEFINES TAUFNDF.
               05 TAUFNDA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TAUFNDI                        PIC X(7).
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
            03 TPESTRI                        PIC X(8).
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
            03 TSTATII                        PIC X(12).
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
            03 ASTATUI                        PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0050
         01 BAN0050O REDEFINES BAN0050I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNR2H                        PIC X.
            03 TVBNR2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVBNR2H                        PIC X.
            03 AVBNR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVBPRZ2H                       PIC X.
            03 AVBPRZ2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIR2H                         PIC X.
            03 ADIR2O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 AVBNAM2H                       PIC X.
            03 AVBNAM2O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVMNR2H                        PIC X.
            03 TVMNR2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVMNR2H                        PIC X.
            03 AVMNR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVMPRZ2H                       PIC X.
            03 AVMPRZ2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AVMNAM2H                       PIC X.
            03 AVMNAM2O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVNR2H                         PIC X.
            03 TVNR2O                         PIC X(11).
            03 FILLER                         PIC X(3).
            03 EVNR2H                         PIC X.
            03 EVNR2O                         PIC X(15).
            03 FILLER                         PIC X(3).
            03 TVORSC2H                       PIC X.
            03 TVORSC2O                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVORSC2H                       PIC X.
            03 AVORSC2O                       PIC X(17).
            03 FILLER                         PIC X(3).
            03 TVEBEGH                        PIC X.
            03 TVEBEGO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EVEBEGH                        PIC X.
            03 EVEBEGO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TEGEBUEH                       PIC X.
            03 TEGEBUEO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 EEGEBUEH                       PIC X.
            03 EEGEBUEO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 TPRODIH                        PIC X.
            03 TPRODIO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EPROD1H                        PIC X.
            03 EPROD1O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TPRODI4H                       PIC X.
            03 TPRODI4O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AEINH1H                        PIC X.
            03 AEINH1O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 TPRODI2H                       PIC X.
            03 TPRODI2O                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 EPROD2H                        PIC X.
            03 EPROD2O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TPRODI3H                       PIC X.
            03 TPRODI3O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AEINH2H                        PIC X.
            03 AEINH2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 TINHH                          PIC X.
            03 TINHO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 EINH1H                         PIC X.
            03 EINH1O                         PIC X(70).
            03 FILLER                         PIC X(3).
            03 EINH2H                         PIC X.
            03 EINH2O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 TKIHVNH                        PIC X.
            03 TKIHVNO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EKIHVNH                        PIC X.
            03 EKIHVNO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 TKIHZNH                        PIC X.
            03 TKIHZNO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EKIHZNH                        PIC X.
            03 EKIHZNO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 TKIHSTRH                       PIC X.
            03 TKIHSTRO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 EKIHSTRH                       PIC X.
            03 EKIHSTRO                       PIC X(45).
            03 FILLER                         PIC X(3).
            03 TKIHPLZH                       PIC X.
            03 TKIHPLZO                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 EKIHPLZH                       PIC X.
            03 EKIHPLZO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 TKIHORTH                       PIC X.
            03 TKIHORTO                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 EKIHORTH                       PIC X.
            03 EKIHORTO                       PIC X(50).
            03 FILLER                         PIC X(3).
            03 TGELDIH                        PIC X.
            03 TGELDIO                        PIC X(13).
            03 FILLER                         PIC X(3).
            03 EGELDIH                        PIC X.
            03 EGELDIO                        PIC X(58).
            03 FILLER                         PIC X(3).
            03 TBICH                          PIC X.
            03 TBICO                          PIC X(4).
            03 FILLER                         PIC X(3).
            03 EBICH                          PIC X.
            03 EBICO                          PIC X(11).
            03 FILLER                         PIC X(3).
            03 TIBANH                         PIC X.
            03 TIBANO                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 EIBANH                         PIC X.
            03 EIBANO                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 TSEPA-MH                       PIC X.
            03 TSEPA-MO                       PIC X(19).
            03 FILLER                         PIC X(3).
            03 ESEPA-MH                       PIC X.
            03 ESEPA-MO                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 TFACOPTH                       PIC X.
            03 TFACOPTO                       PIC X(14).
            03 FILLER                         PIC X(3).
            03 EFACOPTH                       PIC X.
            03 EFACOPTO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 TUMSATZH                       PIC X.
            03 TUMSATZO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EUMSATZH                       PIC X.
            03 EUMSATZO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 TGELDWH                        PIC X.
            03 TGELDWO                        PIC X(16).
            03 FILLER                         PIC X(3).
            03 EGELDWH                        PIC X.
            03 EGELDWO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TGARANTH                       PIC X.
            03 TGARANTO                       PIC X(19).
            03 FILLER                         PIC X(3).
            03 EGARANTH                       PIC X.
            03 EGARANTO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 TNEGATH                        PIC X.
            03 TNEGATO                        PIC X(18).
            03 FILLER                         PIC X(3).
            03 ENEGATH                        PIC X.
            03 ENEGATO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 THANDH                         PIC X.
            03 THANDO                         PIC X(23).
            03 FILLER                         PIC X(3).
            03 EHANDH                         PIC X.
            03 EHANDO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 TFORMBH                        PIC X.
            03 TFORMBO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EFORMBH                        PIC X.
            03 EFORMBO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TANARTH                        PIC X.
            03 TANARTO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 EANARTH                        PIC X.
            03 EANARTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TAUFNDH                        PIC X.
            03 TAUFNDO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EAUFNDH                        PIC X.
            03 EAUFNDO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TPESTRH                        PIC X.
            03 TPESTRO                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 EPESTRH                        PIC X.
            03 EPESTRO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 TSTATIH                        PIC X.
            03 TSTATIO                        PIC X(12).
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
            03 ASTATUO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).




