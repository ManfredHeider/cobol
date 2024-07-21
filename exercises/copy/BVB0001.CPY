      *   Micro Focus Enterprise Developer for Eclipse  5.0.00256
      *   Micro Focus BMS Screen Painter (ver BMSIDE_5-0-01)
      *   MapSet Name   BVB0001
      *   Date Created  09/27/2022
      *   Time Created  16:05:38

      *  Input Data For Map BVB0001
         01 BVB0001I.
            03 FILLER                         PIC X(12).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 ESTPRZI                        PIC X(1).
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER REDEFINES AINFOZLF.
               05 AINFOZLA                       PIC X.
            03 AINFOZLI                       PIC X(65).
            03 ATA34CFL                       PIC S9(4) COMP.
            03 ATA34CFF                       PIC X.
            03 FILLER REDEFINES ATA34CFF.
               05 ATA34CFA                       PIC X.
            03 ATA34CFI                       PIC X(8).
            03 AAW34CFL                       PIC S9(4) COMP.
            03 AAW34CFF                       PIC X.
            03 FILLER REDEFINES AAW34CFF.
               05 AAW34CFA                       PIC X.
            03 AAW34CFI                       PIC X(1).
            03 EAW34CFL                       PIC S9(4) COMP.
            03 EAW34CFF                       PIC X.
            03 FILLER REDEFINES EAW34CFF.
               05 EAW34CFA                       PIC X.
            03 EAW34CFI                       PIC X(1).
            03 ATWMAANL                       PIC S9(4) COMP.
            03 ATWMAANF                       PIC X.
            03 FILLER REDEFINES ATWMAANF.
               05 ATWMAANA                       PIC X.
            03 ATWMAANI                       PIC X(31).
            03 AWMAANL                        PIC S9(4) COMP.
            03 AWMAANF                        PIC X.
            03 FILLER REDEFINES AWMAANF.
               05 AWMAANA                        PIC X.
            03 AWMAANI                        PIC X(1).
            03 EWMAANL                        PIC S9(4) COMP.
            03 EWMAANF                        PIC X.
            03 FILLER REDEFINES EWMAANF.
               05 EWMAANA                        PIC X.
            03 EWMAANI                        PIC X(1).
            03 AANREDEL                       PIC S9(4) COMP.
            03 AANREDEF                       PIC X.
            03 FILLER REDEFINES AANREDEF.
               05 AANREDEA                       PIC X.
            03 AANREDEI                       PIC X(1).
            03 EANREDEL                       PIC S9(4) COMP.
            03 EANREDEF                       PIC X.
            03 FILLER REDEFINES EANREDEF.
               05 EANREDEA                       PIC X.
            03 EANREDEI                       PIC X(1).
            03 AAKADL                         PIC S9(4) COMP.
            03 AAKADF                         PIC X.
            03 FILLER REDEFINES AAKADF.
               05 AAKADA                         PIC X.
            03 AAKADI                         PIC X(1).
            03 EAKADL                         PIC S9(4) COMP.
            03 EAKADF                         PIC X.
            03 FILLER REDEFINES EAKADF.
               05 EAKADA                         PIC X.
            03 EAKADI                         PIC X(10).
            03 AADELL                         PIC S9(4) COMP.
            03 AADELF                         PIC X.
            03 FILLER REDEFINES AADELF.
               05 AADELA                         PIC X.
            03 AADELI                         PIC X(1).
            03 EADELL                         PIC S9(4) COMP.
            03 EADELF                         PIC X.
            03 FILLER REDEFINES EADELF.
               05 EADELA                         PIC X.
            03 EADELI                         PIC X(10).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER REDEFINES AVORNAMF.
               05 AVORNAMA                       PIC X.
            03 AVORNAMI                       PIC X(1).
            03 EVORNAML                       PIC S9(4) COMP.
            03 EVORNAMF                       PIC X.
            03 FILLER REDEFINES EVORNAMF.
               05 EVORNAMA                       PIC X.
            03 EVORNAMI                       PIC X(20).
            03 AMANVONL                       PIC S9(4) COMP.
            03 AMANVONF                       PIC X.
            03 FILLER REDEFINES AMANVONF.
               05 AMANVONA                       PIC X.
            03 AMANVONI                       PIC X(1).
            03 EMANVTL                        PIC S9(4) COMP.
            03 EMANVTF                        PIC X.
            03 FILLER REDEFINES EMANVTF.
               05 EMANVTA                        PIC X.
            03 EMANVTI                        PIC X(2).
            03 EMANVML                        PIC S9(4) COMP.
            03 EMANVMF                        PIC X.
            03 FILLER REDEFINES EMANVMF.
               05 EMANVMA                        PIC X.
            03 EMANVMI                        PIC X(2).
            03 EMANVJL                        PIC S9(4) COMP.
            03 EMANVJF                        PIC X.
            03 FILLER REDEFINES EMANVJF.
               05 EMANVJA                        PIC X.
            03 EMANVJI                        PIC X(4).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER REDEFINES ANAMEF.
               05 ANAMEA                         PIC X.
            03 ANAMEI                         PIC X(1).
            03 ENAMEL                         PIC S9(4) COMP.
            03 ENAMEF                         PIC X.
            03 FILLER REDEFINES ENAMEF.
               05 ENAMEA                         PIC X.
            03 ENAMEI                         PIC X(39).
            03 AGEBDATL                       PIC S9(4) COMP.
            03 AGEBDATF                       PIC X.
            03 FILLER REDEFINES AGEBDATF.
               05 AGEBDATA                       PIC X.
            03 AGEBDATI                       PIC X(1).
            03 EGEBTL                         PIC S9(4) COMP.
            03 EGEBTF                         PIC X.
            03 FILLER REDEFINES EGEBTF.
               05 EGEBTA                         PIC X.
            03 EGEBTI                         PIC X(2).
            03 EGEBML                         PIC S9(4) COMP.
            03 EGEBMF                         PIC X.
            03 FILLER REDEFINES EGEBMF.
               05 EGEBMA                         PIC X.
            03 EGEBMI                         PIC X(2).
            03 EGEBJL                         PIC S9(4) COMP.
            03 EGEBJF                         PIC X.
            03 FILLER REDEFINES EGEBJF.
               05 EGEBJA                         PIC X.
            03 EGEBJI                         PIC X(4).
            03 EMAANTPL                       PIC S9(4) COMP.
            03 EMAANTPF                       PIC X.
            03 FILLER REDEFINES EMAANTPF.
               05 EMAANTPA                       PIC X.
            03 EMAANTPI                       PIC X(1).
            03 AMAANVL                        PIC S9(4) COMP.
            03 AMAANVF                        PIC X.
            03 FILLER REDEFINES AMAANVF.
               05 AMAANVA                        PIC X.
            03 AMAANVI                        PIC X(1).
            03 EMAANTL                        PIC S9(4) COMP.
            03 EMAANTF                        PIC X.
            03 FILLER REDEFINES EMAANTF.
               05 EMAANTA                        PIC X.
            03 EMAANTI                        PIC X(2).
            03 EMAANML                        PIC S9(4) COMP.
            03 EMAANMF                        PIC X.
            03 FILLER REDEFINES EMAANMF.
               05 EMAANMA                        PIC X.
            03 EMAANMI                        PIC X(2).
            03 EMAANJL                        PIC S9(4) COMP.
            03 EMAANJF                        PIC X.
            03 FILLER REDEFINES EMAANJF.
               05 EMAANJA                        PIC X.
            03 EMAANJI                        PIC X(4).
            03 AZUSTLL                        PIC S9(4) COMP.
            03 AZUSTLF                        PIC X.
            03 FILLER REDEFINES AZUSTLF.
               05 AZUSTLA                        PIC X.
            03 AZUSTLI                        PIC X(1).
            03 EZUSTLL                        PIC S9(4) COMP.
            03 EZUSTLF                        PIC X.
            03 FILLER REDEFINES EZUSTLF.
               05 EZUSTLA                        PIC X.
            03 EZUSTLI                        PIC X(2).
            03 AGRUNDL                        PIC S9(4) COMP.
            03 AGRUNDF                        PIC X.
            03 FILLER REDEFINES AGRUNDF.
               05 AGRUNDA                        PIC X.
            03 AGRUNDI                        PIC X(1).
            03 EGRUNDL                        PIC S9(4) COMP.
            03 EGRUNDF                        PIC X.
            03 FILLER REDEFINES EGRUNDF.
               05 EGRUNDA                        PIC X.
            03 EGRUNDI                        PIC X(1).
            03 AWBEIL                         PIC S9(4) COMP.
            03 AWBEIF                         PIC X.
            03 FILLER REDEFINES AWBEIF.
               05 AWBEIA                         PIC X.
            03 AWBEII                         PIC X(1).
            03 EWBEIL                         PIC S9(4) COMP.
            03 EWBEIF                         PIC X.
            03 FILLER REDEFINES EWBEIF.
               05 EWBEIA                         PIC X.
            03 EWBEII                         PIC X(40).
            03 ATBETRL                        PIC S9(4) COMP.
            03 ATBETRF                        PIC X.
            03 FILLER REDEFINES ATBETRF.
               05 ATBETRA                        PIC X.
            03 ATBETRI                        PIC X(11).
            03 ABETRSL                        PIC S9(4) COMP.
            03 ABETRSF                        PIC X.
            03 FILLER REDEFINES ABETRSF.
               05 ABETRSA                        PIC X.
            03 ABETRSI                        PIC X(1).
            03 EBETRSL                        PIC S9(4) COMP.
            03 EBETRSF                        PIC X.
            03 FILLER REDEFINES EBETRSF.
               05 EBETRSA                        PIC X.
            03 EBETRSI                        PIC X(1).
            03 ASTRASSL                       PIC S9(4) COMP.
            03 ASTRASSF                       PIC X.
            03 FILLER REDEFINES ASTRASSF.
               05 ASTRASSA                       PIC X.
            03 ASTRASSI                       PIC X(1).
            03 ESTRASSL                       PIC S9(4) COMP.
            03 ESTRASSF                       PIC X.
            03 FILLER REDEFINES ESTRASSF.
               05 ESTRASSA                       PIC X.
            03 ESTRASSI                       PIC X(40).
            03 AGEWADL                        PIC S9(4) COMP.
            03 AGEWADF                        PIC X.
            03 FILLER REDEFINES AGEWADF.
               05 AGEWADA                        PIC X.
            03 AGEWADI                        PIC X(1).
            03 EGEWADL                        PIC S9(4) COMP.
            03 EGEWADF                        PIC X.
            03 FILLER REDEFINES EGEWADF.
               05 EGEWADA                        PIC X.
            03 EGEWADI                        PIC X(1).
            03 APLZAKTL                       PIC S9(4) COMP.
            03 APLZAKTF                       PIC X.
            03 FILLER REDEFINES APLZAKTF.
               05 APLZAKTA                       PIC X.
            03 APLZAKTI                       PIC X(1).
            03 EPLZAKTL                       PIC S9(4) COMP.
            03 EPLZAKTF                       PIC X.
            03 FILLER REDEFINES EPLZAKTF.
               05 EPLZAKTA                       PIC X.
            03 EPLZAKTI                       PIC X(5).
            03 AORTL                          PIC S9(4) COMP.
            03 AORTF                          PIC X.
            03 FILLER REDEFINES AORTF.
               05 AORTA                          PIC X.
            03 AORTI                          PIC X(1).
            03 EORTL                          PIC S9(4) COMP.
            03 EORTF                          PIC X.
            03 FILLER REDEFINES EORTF.
               05 EORTA                          PIC X.
            03 EORTI                          PIC X(40).
            03 AAUSLL                         PIC S9(4) COMP.
            03 AAUSLF                         PIC X.
            03 FILLER REDEFINES AAUSLF.
               05 AAUSLA                         PIC X.
            03 AAUSLI                         PIC X(1).
            03 EAUSLL                         PIC S9(4) COMP.
            03 EAUSLF                         PIC X.
            03 FILLER REDEFINES EAUSLF.
               05 EAUSLA                         PIC X.
            03 EAUSLI                         PIC X(3).
            03 ATELFONL                       PIC S9(4) COMP.
            03 ATELFONF                       PIC X.
            03 FILLER REDEFINES ATELFONF.
               05 ATELFONA                       PIC X.
            03 ATELFONI                       PIC X(1).
            03 ETELFONL                       PIC S9(4) COMP.
            03 ETELFONF                       PIC X.
            03 FILLER REDEFINES ETELFONF.
               05 ETELFONA                       PIC X.
            03 ETELFONI                       PIC X(26).
            03 ATELFAXL                       PIC S9(4) COMP.
            03 ATELFAXF                       PIC X.
            03 FILLER REDEFINES ATELFAXF.
               05 ATELFAXA                       PIC X.
            03 ATELFAXI                       PIC X(1).
            03 ETELFAXL                       PIC S9(4) COMP.
            03 ETELFAXF                       PIC X.
            03 FILLER REDEFINES ETELFAXF.
               05 ETELFAXA                       PIC X.
            03 ETELFAXI                       PIC X(26).
            03 E1MAANPL                       PIC S9(4) COMP.
            03 E1MAANPF                       PIC X.
            03 FILLER REDEFINES E1MAANPF.
               05 E1MAANPA                       PIC X.
            03 E1MAANPI                       PIC X(1).
            03 A1MAANVL                       PIC S9(4) COMP.
            03 A1MAANVF                       PIC X.
            03 FILLER REDEFINES A1MAANVF.
               05 A1MAANVA                       PIC X.
            03 A1MAANVI                       PIC X(1).
            03 E1MAANTL                       PIC S9(4) COMP.
            03 E1MAANTF                       PIC X.
            03 FILLER REDEFINES E1MAANTF.
               05 E1MAANTA                       PIC X.
            03 E1MAANTI                       PIC X(2).
            03 E1MAANML                       PIC S9(4) COMP.
            03 E1MAANMF                       PIC X.
            03 FILLER REDEFINES E1MAANMF.
               05 E1MAANMA                       PIC X.
            03 E1MAANMI                       PIC X(2).
            03 E1MAANJL                       PIC S9(4) COMP.
            03 E1MAANJF                       PIC X.
            03 FILLER REDEFINES E1MAANJF.
               05 E1MAANJA                       PIC X.
            03 E1MAANJI                       PIC X(4).
            03 A1ZUSTLL                       PIC S9(4) COMP.
            03 A1ZUSTLF                       PIC X.
            03 FILLER REDEFINES A1ZUSTLF.
               05 A1ZUSTLA                       PIC X.
            03 A1ZUSTLI                       PIC X(1).
            03 E1ZUSTLL                       PIC S9(4) COMP.
            03 E1ZUSTLF                       PIC X.
            03 FILLER REDEFINES E1ZUSTLF.
               05 E1ZUSTLA                       PIC X.
            03 E1ZUSTLI                       PIC X(2).
            03 A1GRUNDL                       PIC S9(4) COMP.
            03 A1GRUNDF                       PIC X.
            03 FILLER REDEFINES A1GRUNDF.
               05 A1GRUNDA                       PIC X.
            03 A1GRUNDI                       PIC X(1).
            03 E1GRUNDL                       PIC S9(4) COMP.
            03 E1GRUNDF                       PIC X.
            03 FILLER REDEFINES E1GRUNDF.
               05 E1GRUNDA                       PIC X.
            03 E1GRUNDI                       PIC X(1).
            03 A1WBEIL                        PIC S9(4) COMP.
            03 A1WBEIF                        PIC X.
            03 FILLER REDEFINES A1WBEIF.
               05 A1WBEIA                        PIC X.
            03 A1WBEII                        PIC X(1).
            03 E1WBEIL                        PIC S9(4) COMP.
            03 E1WBEIF                        PIC X.
            03 FILLER REDEFINES E1WBEIF.
               05 E1WBEIA                        PIC X.
            03 E1WBEII                        PIC X(40).
            03 AT1BETRL                       PIC S9(4) COMP.
            03 AT1BETRF                       PIC X.
            03 FILLER REDEFINES AT1BETRF.
               05 AT1BETRA                       PIC X.
            03 AT1BETRI                       PIC X(11).
            03 A1BETRSL                       PIC S9(4) COMP.
            03 A1BETRSF                       PIC X.
            03 FILLER REDEFINES A1BETRSF.
               05 A1BETRSA                       PIC X.
            03 A1BETRSI                       PIC X(1).
            03 E1BETRSL                       PIC S9(4) COMP.
            03 E1BETRSF                       PIC X.
            03 FILLER REDEFINES E1BETRSF.
               05 E1BETRSA                       PIC X.
            03 E1BETRSI                       PIC X(1).
            03 A1STRASL                       PIC S9(4) COMP.
            03 A1STRASF                       PIC X.
            03 FILLER REDEFINES A1STRASF.
               05 A1STRASA                       PIC X.
            03 A1STRASI                       PIC X(1).
            03 E1STRASL                       PIC S9(4) COMP.
            03 E1STRASF                       PIC X.
            03 FILLER REDEFINES E1STRASF.
               05 E1STRASA                       PIC X.
            03 E1STRASI                       PIC X(40).
            03 A1GEWADL                       PIC S9(4) COMP.
            03 A1GEWADF                       PIC X.
            03 FILLER REDEFINES A1GEWADF.
               05 A1GEWADA                       PIC X.
            03 A1GEWADI                       PIC X(1).
            03 E1GEWADL                       PIC S9(4) COMP.
            03 E1GEWADF                       PIC X.
            03 FILLER REDEFINES E1GEWADF.
               05 E1GEWADA                       PIC X.
            03 E1GEWADI                       PIC X(1).
            03 A1PLZAKL                       PIC S9(4) COMP.
            03 A1PLZAKF                       PIC X.
            03 FILLER REDEFINES A1PLZAKF.
               05 A1PLZAKA                       PIC X.
            03 A1PLZAKI                       PIC X(1).
            03 E1PLZAKL                       PIC S9(4) COMP.
            03 E1PLZAKF                       PIC X.
            03 FILLER REDEFINES E1PLZAKF.
               05 E1PLZAKA                       PIC X.
            03 E1PLZAKI                       PIC X(5).
            03 A1ORTL                         PIC S9(4) COMP.
            03 A1ORTF                         PIC X.
            03 FILLER REDEFINES A1ORTF.
               05 A1ORTA                         PIC X.
            03 A1ORTI                         PIC X(1).
            03 E1ORTL                         PIC S9(4) COMP.
            03 E1ORTF                         PIC X.
            03 FILLER REDEFINES E1ORTF.
               05 E1ORTA                         PIC X.
            03 E1ORTI                         PIC X(40).
            03 A1AUSLL                        PIC S9(4) COMP.
            03 A1AUSLF                        PIC X.
            03 FILLER REDEFINES A1AUSLF.
               05 A1AUSLA                        PIC X.
            03 A1AUSLI                        PIC X(1).
            03 E1AUSLL                        PIC S9(4) COMP.
            03 E1AUSLF                        PIC X.
            03 FILLER REDEFINES E1AUSLF.
               05 E1AUSLA                        PIC X.
            03 E1AUSLI                        PIC X(3).
            03 A1TLFONL                       PIC S9(4) COMP.
            03 A1TLFONF                       PIC X.
            03 FILLER REDEFINES A1TLFONF.
               05 A1TLFONA                       PIC X.
            03 A1TLFONI                       PIC X(1).
            03 E1TLFONL                       PIC S9(4) COMP.
            03 E1TLFONF                       PIC X.
            03 FILLER REDEFINES E1TLFONF.
               05 E1TLFONA                       PIC X.
            03 E1TLFONI                       PIC X(26).
            03 A1TLFAXL                       PIC S9(4) COMP.
            03 A1TLFAXF                       PIC X.
            03 FILLER REDEFINES A1TLFAXF.
               05 A1TLFAXA                       PIC X.
            03 A1TLFAXI                       PIC X(1).
            03 E1TLFAXL                       PIC S9(4) COMP.
            03 E1TLFAXF                       PIC X.
            03 FILLER REDEFINES E1TLFAXF.
               05 E1TLFAXA                       PIC X.
            03 E1TLFAXI                       PIC X(26).

      *  Output Data For Map BVB0001
         01 BVB0001O REDEFINES BVB0001I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(3).
            03 ATA34CFO                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 AAW34CFO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EAW34CFO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATWMAANO                       PIC X(31).
            03 FILLER                         PIC X(3).
            03 AWMAANO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EWMAANO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AANREDEO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EANREDEO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AAKADO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EAKADO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 AADELO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EADELO                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 AVORNAMO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EVORNAMO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 AMANVONO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EMANVTO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EMANVMO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EMANVJO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ANAMEO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ENAMEO                         PIC X(39).
            03 FILLER                         PIC X(3).
            03 AGEBDATO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EGEBTO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGEBMO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EGEBJO                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 EMAANTPO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AMAANVO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EMAANTO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EMAANMO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 EMAANJO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AZUSTLO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EZUSTLO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 AGRUNDO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EGRUNDO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AWBEIO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EWBEIO                         PIC X(40).
            03 FILLER                         PIC X(3).
            03 ATBETRO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 ABETRSO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBETRSO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASTRASSO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ESTRASSO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 AGEWADO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 EGEWADO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 APLZAKTO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 EPLZAKTO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AORTO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EORTO                          PIC X(40).
            03 FILLER                         PIC X(3).
            03 AAUSLO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 EAUSLO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ATELFONO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ETELFONO                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 ATELFAXO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ETELFAXO                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 E1MAANPO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A1MAANVO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1MAANTO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 E1MAANMO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 E1MAANJO                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 A1ZUSTLO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1ZUSTLO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 A1GRUNDO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1GRUNDO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A1WBEIO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1WBEIO                        PIC X(40).
            03 FILLER                         PIC X(3).
            03 AT1BETRO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 A1BETRSO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1BETRSO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A1STRASO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1STRASO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 A1GEWADO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1GEWADO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A1PLZAKO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1PLZAKO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 A1ORTO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1ORTO                         PIC X(40).
            03 FILLER                         PIC X(3).
            03 A1AUSLO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1AUSLO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 A1TLFONO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1TLFONO                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 A1TLFAXO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 E1TLFAXO                       PIC X(26).

