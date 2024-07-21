      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0015.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBSL0015.CPv  $
      *
      *   Rev 2.2   Aug 10 2017 16:59:32   t13ae13
      *ASL00567: LUCY2 für Eliteclubbonus SL13 (swl) 
      *
      *   Rev 2.1   Mar 17 2017 08:39:42   t13ae13
      *ASL00558: Dialog für Elite-Club-Bonus
      *
      *   Rev 2.0   Nov 04 2016 12:34:02   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BSL0015
      *   Date Created  07/12/2017
      *   Time Created  17:41:24

      *  Input Data For Map BSL0015
         01 BSL0015I.
            03 FILLER                         PIC X(12).
            03 ADIRNRL                        PIC S9(4) COMP.
            03 ADIRNRF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRNRI                        PIC X(3).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ESTPRZI                        PIC X(1).
            03 AMSBEZL                        PIC S9(4) COMP.
            03 AMSBEZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AMSBEZI                        PIC X(4).
            03 AVBPBEZL                       PIC S9(4) COMP.
            03 AVBPBEZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPBEZI                       PIC X(2).
            03 ELETMONL                       PIC S9(4) COMP.
            03 ELETMONF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ELETMONI                       PIC X(3).
            03 EMONVONL                       PIC S9(4) COMP.
            03 EMONVONF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONVONI                       PIC X(4).
            03 EMONBISL                       PIC S9(4) COMP.
            03 EMONBISF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONBISI                       PIC X(4).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(31).
            03 AADMCLL                        PIC S9(4) COMP.
            03 AADMCLF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AADMCLI                        PIC X(1).
            03 AKAPCLL                        PIC S9(4) COMP.
            03 AKAPCLF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AKAPCLI                        PIC X(1).
            03 AELICLL                        PIC S9(4) COMP.
            03 AELICLF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AELICLI                        PIC X(1).
            03 ASTSTATL                       PIC S9(4) COMP.
            03 ASTSTATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASTSTATI                       PIC X(2).
            03 AKOPFL                         PIC S9(4) COMP.
            03 AKOPFF                         PIC X.
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
               05 AECBALTL                       PIC S9(4) COMP.
               05 AECBALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AECBALTI                       PIC X(9).
               05 EECBNEUL                       PIC S9(4) COMP.
               05 EECBNEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EECBNEUI                       PIC X(9).
               05 AECBPFAL                       PIC S9(4) COMP.
               05 AECBPFAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AECBPFAI                       PIC X(9).
               05 EECBPFNL                       PIC S9(4) COMP.
               05 EECBPFNF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EECBPFNI                       PIC X(9).
               05 ABESCHL                        PIC S9(4) COMP.
               05 ABESCHF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHI                        PIC X(1).
               05 EANSPRGL                       PIC S9(4) COMP.
               05 EANSPRGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EANSPRGI                       PIC X(1).
               05 AANSPRSL                       PIC S9(4) COMP.
               05 AANSPRSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANSPRSI                       PIC X(10).
            03 ASUMECBL                       PIC S9(4) COMP.
            03 ASUMECBF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASUMECBI                       PIC X(9).
            03 ASUMMPFL                       PIC S9(4) COMP.
            03 ASUMMPFF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASUMMPFI                       PIC X(9).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  Output Data For Map BSL0015
         01 BSL0015O REDEFINES BSL0015I.
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
            03 AADMCLA                        PIC X.
            03 AADMCLH                        PIC X.
            03 AADMCLO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 AKAPCLA                        PIC X.
            03 AKAPCLH                        PIC X.
            03 AKAPCLO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 AELICLA                        PIC X.
            03 AELICLH                        PIC X.
            03 AELICLO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASTSTATA                       PIC X.
            03 ASTSTATH                       PIC X.
            03 ASTSTATO                       PIC X(2).
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
               05 AECBALTA                       PIC X.
               05 AECBALTH                       PIC X.
               05 AECBALTO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 EECBNEUA                       PIC X.
               05 EECBNEUH                       PIC X.
               05 EECBNEUO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 AECBPFAA                       PIC X.
               05 AECBPFAH                       PIC X.
               05 AECBPFAO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 EECBPFNA                       PIC X.
               05 EECBPFNH                       PIC X.
               05 EECBPFNO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 ABESCHA                        PIC X.
               05 ABESCHH                        PIC X.
               05 ABESCHO                        PIC X(1).
               05 FILLER                         PIC X(2).
               05 EANSPRGA                       PIC X.
               05 EANSPRGH                       PIC X.
               05 EANSPRGO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 AANSPRSA                       PIC X.
               05 AANSPRSH                       PIC X.
               05 AANSPRSO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 ASUMECBA                       PIC X.
            03 ASUMECBH                       PIC X.
            03 ASUMECBO                       PIC X(9).
            03 FILLER                         PIC X(2).
            03 ASUMMPFA                       PIC X.
            03 ASUMMPFH                       PIC X.
            03 ASUMMPFO                       PIC X(9).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).


