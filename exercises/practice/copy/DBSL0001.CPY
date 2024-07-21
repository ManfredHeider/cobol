      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  11.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBSL0001.CPv  $
      *
      *   Rev 2.2   Sep 15 2016 12:28:58   t13ae13
      *AVB01400: Projekt Lucy2 - Anpassung SL02 (SWL)
      *
      *   Rev 2.1   Apr 23 2003 15:56:12   t13ex22
      *ASL00156: Neuer Dialog SL01
      *
      *   Rev 2.0   Mar 11 2003 16:27:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BSL0001
      *   Date Created  06/13/2016
      *   Time Created  10:30:33

      *  Input Data For Map BSL0001
         01 BSL0001I.
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
            03 AKOPFI                         PIC X(40).
            03 DFHG001I                       OCCURS 3 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 AMONATL                        PIC S9(4) COMP.
               05 AMONATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AMONATI                        PIC X(7).
               05 ABOZALTL                       PIC S9(4) COMP.
               05 ABOZALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABOZALTI                       PIC X(8).
               05 AWBALTL                        PIC S9(4) COMP.
               05 AWBALTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AWBALTI                        PIC X(3).
               05 ABESCHRL                       PIC S9(4) COMP.
               05 ABESCHRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHRI                       PIC X(1).
               05 EBOZNEUL                       PIC S9(4) COMP.
               05 EBOZNEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EBOZNEUI                       PIC X(7).
               05 AWBNEUL                        PIC S9(4) COMP.
               05 AWBNEUF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AWBNEUI                        PIC X(3).
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
            03 AKOPF2I                        PIC X(40).
            03 DFHG002I                       OCCURS 3 TIMES.
               05 EFCPL                          PIC S9(4) COMP.
               05 EFCPF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCPI                          PIC X(1).
               05 AMONATPL                       PIC S9(4) COMP.
               05 AMONATPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AMONATPI                       PIC X(7).
               05 ABOZALPL                       PIC S9(4) COMP.
               05 ABOZALPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABOZALPI                       PIC X(8).
               05 AWBALTPL                       PIC S9(4) COMP.
               05 AWBALTPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AWBALTPI                       PIC X(3).
               05 ABESCHPL                       PIC S9(4) COMP.
               05 ABESCHPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHPI                       PIC X(1).
               05 EBOZNEPL                       PIC S9(4) COMP.
               05 EBOZNEPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EBOZNEPI                       PIC X(7).
               05 AWBNEUPL                       PIC S9(4) COMP.
               05 AWBNEUPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AWBNEUPI                       PIC X(3).
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

      *  Output Data For Map BSL0001
         01 BSL0001O REDEFINES BSL0001I.
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
            03 AKOPFO                         PIC X(40).
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
               05 ABOZALTA                       PIC X.
               05 ABOZALTH                       PIC X.
               05 ABOZALTO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 AWBALTA                        PIC X.
               05 AWBALTH                        PIC X.
               05 AWBALTO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 ABESCHRA                       PIC X.
               05 ABESCHRH                       PIC X.
               05 ABESCHRO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 EBOZNEUA                       PIC X.
               05 EBOZNEUH                       PIC X.
               05 EBOZNEUO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 AWBNEUA                        PIC X.
               05 AWBNEUH                        PIC X.
               05 AWBNEUO                        PIC X(3).
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
            03 AKOPF2O                        PIC X(40).
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
               05 ABOZALPA                       PIC X.
               05 ABOZALPH                       PIC X.
               05 ABOZALPO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 AWBALTPA                       PIC X.
               05 AWBALTPH                       PIC X.
               05 AWBALTPO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ABESCHPA                       PIC X.
               05 ABESCHPH                       PIC X.
               05 ABESCHPO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 EBOZNEPA                       PIC X.
               05 EBOZNEPH                       PIC X.
               05 EBOZNEPO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 AWBNEUPA                       PIC X.
               05 AWBNEUPH                       PIC X.
               05 AWBNEUPO                       PIC X(3).
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


