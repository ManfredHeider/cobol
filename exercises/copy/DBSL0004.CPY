      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBSL0004.CPv  $
      *
      *   Rev 2.1   Sep 22 2004 12:39:46   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 20 2004 13:28:30   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   BSL0004
      *   Date Created  08/20/2004
      *   Time Created  07:32:38

      *  Input Data For Map BSL0004
         01 BSL0004I.
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
               05 ALZBALTL                       PIC S9(4) COMP.
               05 ALZBALTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ALZBALTI                       PIC X(8).
               05 AWBALTL                        PIC S9(4) COMP.
               05 AWBALTF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AWBALTI                        PIC X(3).
               05 ABESCHRL                       PIC S9(4) COMP.
               05 ABESCHRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABESCHRI                       PIC X(1).
               05 ELZBNEUL                       PIC S9(4) COMP.
               05 ELZBNEUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ELZBNEUI                       PIC X(7).
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
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  Output Data For Map BSL0004
         01 BSL0004O REDEFINES BSL0004I.
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
               05 ALZBALTA                       PIC X.
               05 ALZBALTH                       PIC X.
               05 ALZBALTO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 AWBALTA                        PIC X.
               05 AWBALTH                        PIC X.
               05 AWBALTO                        PIC X(3).
               05 FILLER                         PIC X(2).
               05 ABESCHRA                       PIC X.
               05 ABESCHRH                       PIC X.
               05 ABESCHRO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ELZBNEUA                       PIC X.
               05 ELZBNEUH                       PIC X.
               05 ELZBNEUO                       PIC X(7).
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
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).


