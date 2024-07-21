      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0003.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DBSL0003.CPv  $
      *
      *   Rev 2.3   Sep 15 2016 12:29:18   t13ae13
      *AVB01400: Projekt Lucy2 - Anpassung SL02 (SWL)
      *
      *   Rev 2.2   Nov 29 2010 15:38:18   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.1   Sep 22 2004 12:39:36   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 20 2004 13:28:12   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BSL0003
      *   Date Created  06/22/2016
      *   Time Created  12:15:20

      *  Input Data For Map BSL0003
         01 BSL0003I.
            03 FILLER                         PIC X(12).
            03 ESTL                           PIC S9(4) COMP.
            03 ESTF                           PIC X.
            03 FILLER                         PIC X(1).
            03 ESTI                           PIC X(7).
            03 ESTPL                          PIC S9(4) COMP.
            03 ESTPF                          PIC X.
            03 FILLER                         PIC X(1).
            03 ESTPI                          PIC X(1).
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AINFOZLI                       PIC X(65).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(20).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVORNAMI                       PIC X(15).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ABTRNAMI                       PIC X(16).
            03 EQUARTL                        PIC S9(4) COMP.
            03 EQUARTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EQUARTI                        PIC X(4).
            03 AKOPFL                         PIC S9(4) COMP.
            03 AKOPFF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AKOPFI                         PIC X(46).
            03 DFHG001I                       OCCURS 3 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 AMONATL                        PIC S9(4) COMP.
               05 AMONATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AMONATI                        PIC X(7).
               05 ALZPALTL                       PIC S9(4) COMP.
               05 ALZPALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ALZPALTI                       PIC X(11).
               05 AWPALTL                        PIC S9(4) COMP.
               05 AWPALTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AWPALTI                        PIC X(3).
               05 ABESCHRL                       PIC S9(4) COMP.
               05 ABESCHRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHRI                       PIC X(1).
               05 ELZPNEUL                       PIC S9(4) COMP.
               05 ELZPNEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ELZPNEUI                       PIC X(10).
               05 AWPNEUL                        PIC S9(4) COMP.
               05 AWPNEUF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AWPNEUI                        PIC X(3).
               05 EANSPRGL                       PIC S9(4) COMP.
               05 EANSPRGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EANSPRGI                       PIC X(1).
               05 AANSPRSL                       PIC S9(4) COMP.
               05 AANSPRSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANSPRSI                       PIC X(10).
            03 AKOPF2L                        PIC S9(4) COMP.
            03 AKOPF2F                        PIC X.
            03 FILLER                         PIC X(1).
            03 AKOPF2I                        PIC X(46).
            03 DFHG002I                       OCCURS 3 TIMES.
               05 EFCPL                          PIC S9(4) COMP.
               05 EFCPF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCPI                          PIC X(1).
               05 AMONATPL                       PIC S9(4) COMP.
               05 AMONATPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AMONATPI                       PIC X(7).
               05 ALZPALPL                       PIC S9(4) COMP.
               05 ALZPALPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ALZPALPI                       PIC X(11).
               05 AWPALTPL                       PIC S9(4) COMP.
               05 AWPALTPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AWPALTPI                       PIC X(3).
               05 ABESCHPL                       PIC S9(4) COMP.
               05 ABESCHPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHPI                       PIC X(1).
               05 ELZPNEPL                       PIC S9(4) COMP.
               05 ELZPNEPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ELZPNEPI                       PIC X(10).
               05 AWPNEUPL                       PIC S9(4) COMP.
               05 AWPNEUPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AWPNEUPI                       PIC X(3).
               05 EANSPGPL                       PIC S9(4) COMP.
               05 EANSPGPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EANSPGPI                       PIC X(1).
               05 AANSPSPL                       PIC S9(4) COMP.
               05 AANSPSPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANSPSPI                       PIC X(10).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  Output Data For Map BSL0003
         01 BSL0003O REDEFINES BSL0003I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 ESTA                           PIC X.
            03 ESTH                           PIC X.
            03 ESTO                           PIC X(7).
            03 FILLER                         PIC X(2).
            03 ESTPA                          PIC X.
            03 ESTPH                          PIC X.
            03 ESTPO                          PIC X(1).
            03 FILLER                         PIC X(2).
            03 AINFOZLA                       PIC X.
            03 AINFOZLH                       PIC X.
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(20).
            03 FILLER                         PIC X(2).
            03 AVORNAMA                       PIC X.
            03 AVORNAMH                       PIC X.
            03 AVORNAMO                       PIC X(15).
            03 FILLER                         PIC X(2).
            03 ABTRNAMA                       PIC X.
            03 ABTRNAMH                       PIC X.
            03 ABTRNAMO                       PIC X(16).
            03 FILLER                         PIC X(2).
            03 EQUARTA                        PIC X.
            03 EQUARTH                        PIC X.
            03 EQUARTO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 AKOPFA                         PIC X.
            03 AKOPFH                         PIC X.
            03 AKOPFO                         PIC X(46).
            03 DFHG001O OCCURS 3.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 AMONATA                        PIC X.
               05 AMONATH                        PIC X.
               05 AMONATO                        PIC X(7).
               05 FILLER                         PIC X(2).
               05 ALZPALTA                       PIC X.
               05 ALZPALTH                       PIC X.
               05 ALZPALTO                       PIC X(11).
               05 FILLER                         PIC X(2).
               05 AWPALTA                        PIC X.
               05 AWPALTH                        PIC X.
               05 AWPALTO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 ABESCHRA                       PIC X.
               05 ABESCHRH                       PIC X.
               05 ABESCHRO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ELZPNEUA                       PIC X.
               05 ELZPNEUH                       PIC X.
               05 ELZPNEUO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AWPNEUA                        PIC X.
               05 AWPNEUH                        PIC X.
               05 AWPNEUO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 EANSPRGA                       PIC X.
               05 EANSPRGH                       PIC X.
               05 EANSPRGO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 AANSPRSA                       PIC X.
               05 AANSPRSH                       PIC X.
               05 AANSPRSO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AKOPF2A                        PIC X.
            03 AKOPF2H                        PIC X.
            03 AKOPF2O                        PIC X(46).
            03 DFHG002O OCCURS 3.
               05 FILLER                         PIC X(2).
               05 EFCPA                          PIC X.
               05 EFCPH                          PIC X.
               05 EFCPO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AMONATPA                       PIC X.
               05 AMONATPH                       PIC X.
               05 AMONATPO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 ALZPALPA                       PIC X.
               05 ALZPALPH                       PIC X.
               05 ALZPALPO                       PIC X(11).
               05 FILLER                         PIC X(2).
               05 AWPALTPA                       PIC X.
               05 AWPALTPH                       PIC X.
               05 AWPALTPO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ABESCHPA                       PIC X.
               05 ABESCHPH                       PIC X.
               05 ABESCHPO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ELZPNEPA                       PIC X.
               05 ELZPNEPH                       PIC X.
               05 ELZPNEPO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AWPNEUPA                       PIC X.
               05 AWPNEUPH                       PIC X.
               05 AWPNEUPO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 EANSPGPA                       PIC X.
               05 EANSPGPH                       PIC X.
               05 EANSPGPO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 AANSPSPA                       PIC X.
               05 AANSPSPH                       PIC X.
               05 AANSPSPO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).


