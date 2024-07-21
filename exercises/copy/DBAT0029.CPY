      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAT0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBAT0029.CPv  $
      *
      *   Rev 2.1   Nov 17 2008 14:30:30   t13ex21
      *APA00809: SF21-CPD-Konten Erweiterung
      *
      *   Rev 2.0   Oct 21 2008 10:30:18   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus Mainframe Express  3.0.75
      *   Micro Focus BMS Screen Painter (ver COBC_1-1-37-58)
      *   MapSet Name   BAT0029
      *   Date Created  10/21/2008
      *   Time Created  10:42:28

      *  Input Data For Map BAT0029
         01 BAT0029I.
            03 FILLER                         PIC X(12).
            03 AGSNRL                         PIC S9(4) COMP.
            03 AGSNRF                         PIC X.
            03 AGSNRI                         PIC X(3).
            03 AFCSL                          PIC S9(4) COMP.
            03 AFCSF                          PIC X.
            03 AFCSI                          PIC X(5).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 EFCSI                          PIC X(1).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 AFCSANZI                       PIC X(30).
            03 AGSLBSL                        PIC S9(4) COMP.
            03 AGSLBSF                        PIC X.
            03 AGSLBSI                        PIC X(1).
            03 EGSLBZL                        PIC S9(4) COMP.
            03 EGSLBZF                        PIC X.
            03 EGSLBZI                        PIC X(40).
            03 AGSKBSL                        PIC S9(4) COMP.
            03 AGSKBSF                        PIC X.
            03 AGSKBSI                        PIC X(1).
            03 EGSKBZL                        PIC S9(4) COMP.
            03 EGSKBZF                        PIC X.
            03 EGSKBZI                        PIC X(10).
            03 AUPDFBL                        PIC S9(4) COMP.
            03 AUPDFBF                        PIC X.
            03 AUPDFBI                        PIC X(22).
            03 AUPDDAL                        PIC S9(4) COMP.
            03 AUPDDAF                        PIC X.
            03 AUPDDAI                        PIC X(10).
            03 AGSKB4SL                       PIC S9(4) COMP.
            03 AGSKB4SF                       PIC X.
            03 AGSKB4SI                       PIC X(1).
            03 EGSKBZ4L                       PIC S9(4) COMP.
            03 EGSKBZ4F                       PIC X.
            03 EGSKBZ4I                       PIC X(4).
            03 AUPDIFBL                       PIC S9(4) COMP.
            03 AUPDIFBF                       PIC X.
            03 AUPDIFBI                       PIC X(4).
            03 AUPDUIDL                       PIC S9(4) COMP.
            03 AUPDUIDF                       PIC X.
            03 AUPDUIDI                       PIC X(8).
            03 AAUSLSL                        PIC S9(4) COMP.
            03 AAUSLSF                        PIC X.
            03 AAUSLSI                        PIC X(1).
            03 EAUSLL                         PIC S9(4) COMP.
            03 EAUSLF                         PIC X.
            03 EAUSLI                         PIC X(2).
            03 AALBEZL                        PIC S9(4) COMP.
            03 AALBEZF                        PIC X.
            03 AALBEZI                        PIC X(4).
            03 ABEWSL                         PIC S9(4) COMP.
            03 ABEWSF                         PIC X.
            03 ABEWSI                         PIC X(1).
            03 EBEWL                          PIC S9(4) COMP.
            03 EBEWF                          PIC X.
            03 EBEWI                          PIC X(2).
            03 ABEWBEZL                       PIC S9(4) COMP.
            03 ABEWBEZF                       PIC X.
            03 ABEWBEZI                       PIC X(10).
            03 AWAEHRSL                       PIC S9(4) COMP.
            03 AWAEHRSF                       PIC X.
            03 AWAEHRSI                       PIC X(1).
            03 EWAEHRL                        PIC S9(4) COMP.
            03 EWAEHRF                        PIC X.
            03 EWAEHRI                        PIC X(2).
            03 AWAEBEZL                       PIC S9(4) COMP.
            03 AWAEBEZF                       PIC X.
            03 AWAEBEZI                       PIC X(10).
            03 AEXPORSL                       PIC S9(4) COMP.
            03 AEXPORSF                       PIC X.
            03 AEXPORSI                       PIC X(1).
            03 EEXPORTL                       PIC S9(4) COMP.
            03 EEXPORTF                       PIC X.
            03 EEXPORTI                       PIC X(2).
            03 AEXPBEZL                       PIC S9(4) COMP.
            03 AEXPBEZF                       PIC X.
            03 AEXPBEZI                       PIC X(10).
            03 AVTGESSL                       PIC S9(4) COMP.
            03 AVTGESSF                       PIC X.
            03 AVTGESSI                       PIC X(1).
            03 EVTGESL                        PIC S9(4) COMP.
            03 EVTGESF                        PIC X.
            03 EVTGESI                        PIC X(2).
            03 AVTGBEZL                       PIC S9(4) COMP.
            03 AVTGBEZF                       PIC X.
            03 AVTGBEZI                       PIC X(10).
            03 DFHG001I                       OCCURS 4 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 EFCI                           PIC X(1).
               05 AAENDKZL                       PIC S9(4) COMP.
               05 AAENDKZF                       PIC X.
               05 AAENDKZI                       PIC X(1).
               05 EMANDANL                       PIC S9(4) COMP.
               05 EMANDANF                       PIC X.
               05 EMANDANI                       PIC X(2).
               05 AMANBZL                        PIC S9(4) COMP.
               05 AMANBZF                        PIC X.
               05 AMANBZI                        PIC X(10).
               05 EKONTOL                        PIC S9(4) COMP.
               05 EKONTOF                        PIC X.
               05 EKONTOI                        PIC X(10).
               05 EGKOEINL                       PIC S9(4) COMP.
               05 EGKOEINF                       PIC X.
               05 EGKOEINI                       PIC X(3).
               05 EGKOAUSL                       PIC S9(4) COMP.
               05 EGKOAUSF                       PIC X.
               05 EGKOAUSI                       PIC X(3).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BAT0029
         01 BAT0029O REDEFINES BAT0029I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGSNRA                         PIC X.
            03 AGSNRO                         PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSA                          PIC X.
            03 AFCSO                          PIC X(5).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSO                          PIC X(1).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZO                       PIC X(30).
            03 FILLER                         PIC X(2).
            03 AGSLBSA                        PIC X.
            03 AGSLBSO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 EGSLBZA                        PIC X.
            03 EGSLBZO                        PIC X(40).
            03 FILLER                         PIC X(2).
            03 AGSKBSA                        PIC X.
            03 AGSKBSO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 EGSKBZA                        PIC X.
            03 EGSKBZO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 AUPDFBA                        PIC X.
            03 AUPDFBO                        PIC X(22).
            03 FILLER                         PIC X(2).
            03 AUPDDAA                        PIC X.
            03 AUPDDAO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 AGSKB4SA                       PIC X.
            03 AGSKB4SO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 EGSKBZ4A                       PIC X.
            03 EGSKBZ4O                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AUPDIFBA                       PIC X.
            03 AUPDIFBO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AUPDUIDA                       PIC X.
            03 AUPDUIDO                       PIC X(8).
            03 FILLER                         PIC X(2).
            03 AAUSLSA                        PIC X.
            03 AAUSLSO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 EAUSLA                         PIC X.
            03 EAUSLO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AALBEZA                        PIC X.
            03 AALBEZO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 ABEWSA                         PIC X.
            03 ABEWSO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 EBEWA                          PIC X.
            03 EBEWO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 ABEWBEZA                       PIC X.
            03 ABEWBEZO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AWAEHRSA                       PIC X.
            03 AWAEHRSO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 EWAEHRA                        PIC X.
            03 EWAEHRO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 AWAEBEZA                       PIC X.
            03 AWAEBEZO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AEXPORSA                       PIC X.
            03 AEXPORSO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 EEXPORTA                       PIC X.
            03 EEXPORTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 AEXPBEZA                       PIC X.
            03 AEXPBEZO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AVTGESSA                       PIC X.
            03 AVTGESSO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 EVTGESA                        PIC X.
            03 EVTGESO                        PIC X(2).
            03 FILLER                         PIC X(2).
            03 AVTGBEZA                       PIC X.
            03 AVTGBEZO                       PIC X(10).
            03 DFHG001O OCCURS 4.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 AAENDKZA                       PIC X.
               05 AAENDKZO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 EMANDANA                       PIC X.
               05 EMANDANO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 AMANBZA                        PIC X.
               05 AMANBZO                        PIC X(10).
               05 FILLER                         PIC X(2).
               05 EKONTOA                        PIC X.
               05 EKONTOO                        PIC X(10).
               05 FILLER                         PIC X(2).
               05 EGKOEINA                       PIC X.
               05 EGKOEINO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 EGKOAUSA                       PIC X.
               05 EGKOAUSO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZO                       PIC X(79).

