      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBSL0007.CPv  $
      *
      *   Rev 2.1   Mar 20 2008 11:41:28   t13ex43
      *ASL00298: PGM - Änderungdialog SL05 für KLZ
      *
      *   Rev 2.0   29 Feb 2008 15:14:38   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-58)
      *   MAPSET NAME   BSL0007
      *   DATE CREATED  03/13/2008
      *   TIME CREATED  09:24:26

      *  INPUT DATA FOR MAP BSL0007
         01 BSL0007I.
            03 FILLER                         PIC X(12).
            03 ADIRNRL                        PIC S9(4) COMP.
            03 ADIRNRF                        PIC X.
            03 FILLER REDEFINES ADIRNRF.
               05 ADIRNRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRNRI                        PIC X(3).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTPRZI                        PIC X(1).
            03 AMSBEZL                        PIC S9(4) COMP.
            03 AMSBEZF                        PIC X.
            03 FILLER REDEFINES AMSBEZF.
               05 AMSBEZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AMSBEZI                        PIC X(4).
            03 AVBPBEZL                       PIC S9(4) COMP.
            03 AVBPBEZF                       PIC X.
            03 FILLER REDEFINES AVBPBEZF.
               05 AVBPBEZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPBEZI                       PIC X(2).
            03 ELETMONL                       PIC S9(4) COMP.
            03 ELETMONF                       PIC X.
            03 FILLER REDEFINES ELETMONF.
               05 ELETMONA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ELETMONI                       PIC X(3).
            03 EMONVONL                       PIC S9(4) COMP.
            03 EMONVONF                       PIC X.
            03 FILLER REDEFINES EMONVONF.
               05 EMONVONA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONVONI                       PIC X(4).
            03 EMONBISL                       PIC S9(4) COMP.
            03 EMONBISF                       PIC X.
            03 FILLER REDEFINES EMONBISF.
               05 EMONBISA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONBISI                       PIC X(4).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER REDEFINES AVBNAMEF.
               05 AVBNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(31).
            03 AKOPFL                         PIC S9(4) COMP.
            03 AKOPFF                         PIC X.
            03 FILLER REDEFINES AKOPFF.
               05 AKOPFA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AKOPFI                         PIC X(75).
            03 DFHG001I                       OCCURS 12 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 ETAGL                          PIC S9(4) COMP.
               05 ETAGF                          PIC X.
               05 FILLER                         PIC X(1).
               05 ETAGI                          PIC X(3).
               05 AMONATL                        PIC S9(4) COMP.
               05 AMONATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AMONATI                        PIC X(5).
               05 AKLZALTL                       PIC S9(4) COMP.
               05 AKLZALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKLZALTI                       PIC X(9).
               05 EKLZNEUL                       PIC S9(4) COMP.
               05 EKLZNEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EKLZNEUI                       PIC X(9).
               05 ABESCHL                        PIC S9(4) COMP.
               05 ABESCHF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHI                        PIC X(1).
               05 ABVOALTL                       PIC S9(4) COMP.
               05 ABVOALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABVOALTI                       PIC X(12).
               05 EBVONEUL                       PIC S9(4) COMP.
               05 EBVONEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EBVONEUI                       PIC X(12).
               05 EANSPRGL                       PIC S9(4) COMP.
               05 EANSPRGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EANSPRGI                       PIC X(1).
               05 AANSPRSL                       PIC S9(4) COMP.
               05 AANSPRSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANSPRSI                       PIC X(10).
            03 ASUMKLZL                       PIC S9(4) COMP.
            03 ASUMKLZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASUMKLZI                       PIC X(9).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  OUTPUT DATA FOR MAP BSL0007
         01 BSL0007O REDEFINES BSL0007I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 ADIRNRA                        PIC X.
            03 ADIRNRH                        PIC X.
            03 ADIRNRO                        PIC X(3).
            03 FILLER                         PIC X(2).
            03 ESTNRA                         PIC X.
            03 ESTNRH                         PIC X.
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(2).
            03 ESTPRZA                        PIC X.
            03 ESTPRZH                        PIC X.
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 AMSBEZA                        PIC X.
            03 AMSBEZH                        PIC X.
            03 AMSBEZO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 AVBPBEZA                       PIC X.
            03 AVBPBEZH                       PIC X.
            03 AVBPBEZO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 ELETMONA                       PIC X.
            03 ELETMONH                       PIC X.
            03 ELETMONO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 EMONVONA                       PIC X.
            03 EMONVONH                       PIC X.
            03 EMONVONO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EMONBISA                       PIC X.
            03 EMONBISH                       PIC X.
            03 EMONBISO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AVBNAMEA                       PIC X.
            03 AVBNAMEH                       PIC X.
            03 AVBNAMEO                       PIC X(31).
            03 FILLER                         PIC X(2).
            03 AKOPFA                         PIC X.
            03 AKOPFH                         PIC X.
            03 AKOPFO                         PIC X(75).
            03 DFHG001O OCCURS 12.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 ETAGA                          PIC X.
               05 ETAGH                          PIC X.
               05 ETAGO                          PIC X(3).
               05 FILLER                         PIC X(2).
               05 AMONATA                        PIC X.
               05 AMONATH                        PIC X.
               05 AMONATO                        PIC X(5).
               05 FILLER                         PIC X(2).
               05 AKLZALTA                       PIC X.
               05 AKLZALTH                       PIC X.
               05 AKLZALTO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 EKLZNEUA                       PIC X.
               05 EKLZNEUH                       PIC X.
               05 EKLZNEUO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 ABESCHA                        PIC X.
               05 ABESCHH                        PIC X.
               05 ABESCHO                        PIC X(1).
               05 FILLER                         PIC X(2).
               05 ABVOALTA                       PIC X.
               05 ABVOALTH                       PIC X.
               05 ABVOALTO                       PIC X(12).
               05 FILLER                         PIC X(2).
               05 EBVONEUA                       PIC X.
               05 EBVONEUH                       PIC X.
               05 EBVONEUO                       PIC X(12).
               05 FILLER                         PIC X(2).
               05 EANSPRGA                       PIC X.
               05 EANSPRGH                       PIC X.
               05 EANSPRGO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 AANSPRSA                       PIC X.
               05 AANSPRSH                       PIC X.
               05 AANSPRSO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 ASUMKLZA                       PIC X.
            03 ASUMKLZH                       PIC X.
            03 ASUMKLZO                       PIC X(9).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).

