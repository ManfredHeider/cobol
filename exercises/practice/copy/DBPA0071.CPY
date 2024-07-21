      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.02.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0071.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBPA0071.CPv  $
      *
      *   Rev 2.3   Jun 07 2019 15:52:56   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.2   Jun 07 2019 12:23:42   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.1   Jun 05 2019 11:39:34   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.0   Feb 13 2019 16:02:32   u130076
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  Input Data For Map BPA0071
         01 BPA0071I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER REDEFINES AGESF.
               05 AGESA                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER REDEFINES AGESBMF.
               05 AGESBMA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 AVNRL                          PIC S9(4) COMP.
            03 AVNRF                          PIC X.
            03 FILLER REDEFINES AVNRF.
               05 AVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 AVNRI                          PIC X(15).
            03 APINFBL                        PIC S9(4) COMP.
            03 APINFBF                        PIC X.
            03 FILLER REDEFINES APINFBF.
               05 APINFBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 APINFBI                        PIC X(4).
            03 APINL                          PIC S9(4) COMP.
            03 APINF                          PIC X.
            03 FILLER REDEFINES APINF.
               05 APINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 APINI                          PIC X(9).
            03 APRODSLL                       PIC S9(4) COMP.
            03 APRODSLF                       PIC X.
            03 FILLER REDEFINES APRODSLF.
               05 APRODSLA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRODSLI                       PIC X(3).
            03 AORDNRL                        PIC S9(4) COMP.
            03 AORDNRF                        PIC X.
            03 FILLER REDEFINES AORDNRF.
               05 AORDNRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AORDNRI                        PIC X(2).
            03 AFAEVDTL                       PIC S9(4) COMP.
            03 AFAEVDTF                       PIC X.
            03 FILLER REDEFINES AFAEVDTF.
               05 AFAEVDTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFAEVDTI                       PIC X(10).
            03 AFAEBDTL                       PIC S9(4) COMP.
            03 AFAEBDTF                       PIC X.
            03 FILLER REDEFINES AFAEBDTF.
               05 AFAEBDTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFAEBDTI                       PIC X(10).
            03 AFOLGNRL                       PIC S9(4) COMP.
            03 AFOLGNRF                       PIC X.
            03 FILLER REDEFINES AFOLGNRF.
               05 AFOLGNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFOLGNRI                       PIC X(3).
            03 ANAMEFBL                       PIC S9(4) COMP.
            03 ANAMEFBF                       PIC X.
            03 FILLER REDEFINES ANAMEFBF.
               05 ANAMEFBA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEFBI                       PIC X(5).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER REDEFINES ANAMEF.
               05 ANAMEA                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 AHISTNRL                       PIC S9(4) COMP.
            03 AHISTNRF                       PIC X.
            03 FILLER REDEFINES AHISTNRF.
               05 AHISTNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHISTNRI                       PIC X(3).
            03 APRSUFBL                       PIC S9(4) COMP.
            03 APRSUFBF                       PIC X.
            03 FILLER REDEFINES APRSUFBF.
               05 APRSUFBA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRSUFBI                       PIC X(24).
            03 APRTASUL                       PIC S9(4) COMP.
            03 APRTASUF                       PIC X.
            03 FILLER REDEFINES APRTASUF.
               05 APRTASUA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRTASUI                       PIC X(13).
            03 APRRAFBL                       PIC S9(4) COMP.
            03 APRRAFBF                       PIC X.
            03 FILLER REDEFINES APRRAFBF.
               05 APRRAFBA                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRRAFBI                       PIC X(24).
            03 EPRRAT1L                       PIC S9(4) COMP.
            03 EPRRAT1F                       PIC X.
            03 FILLER REDEFINES EPRRAT1F.
               05 EPRRAT1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPRRAT1I                       PIC X(2).
            03 APRRFB2L                       PIC S9(4) COMP.
            03 APRRFB2F                       PIC X.
            03 FILLER REDEFINES APRRFB2F.
               05 APRRFB2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRRFB2I                       PIC X(1).
            03 APRRAT2L                       PIC S9(4) COMP.
            03 APRRAT2F                       PIC X.
            03 FILLER REDEFINES APRRAT2F.
               05 APRRAT2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRRAT2I                       PIC X(4).
            03 AHFTFB1L                       PIC S9(4) COMP.
            03 AHFTFB1F                       PIC X.
            03 FILLER REDEFINES AHFTFB1F.
               05 AHFTFB1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHFTFB1I                       PIC X(12).
            03 AHFTFB2L                       PIC S9(4) COMP.
            03 AHFTFB2F                       PIC X.
            03 FILLER REDEFINES AHFTFB2F.
               05 AHFTFB2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHFTFB2I                       PIC X(5).
            03 AHFTVONL                       PIC S9(4) COMP.
            03 AHFTVONF                       PIC X.
            03 FILLER REDEFINES AHFTVONF.
               05 AHFTVONA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHFTVONI                       PIC X(10).
            03 AHFTFB3L                       PIC S9(4) COMP.
            03 AHFTFB3F                       PIC X.
            03 FILLER REDEFINES AHFTFB3F.
               05 AHFTFB3A                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHFTFB3I                       PIC X(3).
            03 AHFTBISL                       PIC S9(4) COMP.
            03 AHFTBISF                       PIC X.
            03 FILLER REDEFINES AHFTBISF.
               05 AHFTBISA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AHFTBISI                       PIC X(10).
            03 AKONDBML                       PIC S9(4) COMP.
            03 AKONDBMF                       PIC X.
            03 FILLER REDEFINES AKONDBMF.
               05 AKONDBMA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AKONDBMI                       PIC X(40).
            03 ATNRFBL                        PIC S9(4) COMP.
            03 ATNRFBF                        PIC X.
            03 FILLER REDEFINES ATNRFBF.
               05 ATNRFBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATNRFBI                        PIC X(24).
            03 ATNRPNL                        PIC S9(4) COMP.
            03 ATNRPNF                        PIC X.
            03 FILLER REDEFINES ATNRPNF.
               05 ATNRPNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATNRPNI                        PIC X(20).
            03 AABRDATL                       PIC S9(4) COMP.
            03 AABRDATF                       PIC X.
            03 FILLER REDEFINES AABRDATF.
               05 AABRDATA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRDATI                       PIC X(10).
            03 AUPDDATL                       PIC S9(4) COMP.
            03 AUPDDATF                       PIC X.
            03 FILLER REDEFINES AUPDDATF.
               05 AUPDDATA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUPDDATI                       PIC X(10).
            03 AUPDUIDL                       PIC S9(4) COMP.
            03 AUPDUIDF                       PIC X.
            03 FILLER REDEFINES AUPDUIDF.
               05 AUPDUIDA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUPDUIDI                       PIC X(8).

      *  Output Data For Map BPA0071
         01 BPA0071O REDEFINES BPA0071I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AVNRH                          PIC X.
            03 AVNRO                          PIC X(15).
            03 FILLER                         PIC X(3).
            03 APINFBH                        PIC X.
            03 APINFBO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 APINH                          PIC X.
            03 APINO                          PIC X(9).
            03 FILLER                         PIC X(3).
            03 APRODSLH                       PIC X.
            03 APRODSLO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AORDNRH                        PIC X.
            03 AORDNRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 AFAEVDTH                       PIC X.
            03 AFAEVDTO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AFAEBDTH                       PIC X.
            03 AFAEBDTO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AFOLGNRH                       PIC X.
            03 AFOLGNRO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ANAMEFBH                       PIC X.
            03 ANAMEFBO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 FILLER                         PIC X(3).
            03 AHISTNRH                       PIC X.
            03 AHISTNRO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 APRSUFBH                       PIC X.
            03 APRSUFBO                       PIC X(24).
            03 FILLER                         PIC X(3).
            03 APRTASUH                       PIC X.
            03 APRTASUO                       PIC X(13).
            03 FILLER                         PIC X(3).
            03 APRRAFBH                       PIC X.
            03 APRRAFBO                       PIC X(24).
            03 FILLER                         PIC X(3).
            03 EPRRAT1H                       PIC X.
            03 EPRRAT1O                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 APRRFB2H                       PIC X.
            03 APRRFB2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 APRRAT2H                       PIC X.
            03 APRRAT2O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 AHFTFB1H                       PIC X.
            03 AHFTFB1O                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 AHFTFB2H                       PIC X.
            03 AHFTFB2O                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AHFTVONH                       PIC X.
            03 AHFTVONO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AHFTFB3H                       PIC X.
            03 AHFTFB3O                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 AHFTBISH                       PIC X.
            03 AHFTBISO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AKONDBMH                       PIC X.
            03 AKONDBMO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 ATNRFBH                        PIC X.
            03 ATNRFBO                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ATNRPNH                        PIC X.
            03 ATNRPNO                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 AABRDATH                       PIC X.
            03 AABRDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AUPDDATH                       PIC X.
            03 AUPDDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AUPDUIDH                       PIC X.
            03 AUPDUIDO                       PIC X(8).


