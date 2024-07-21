      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0014.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DBSL0014.CPv  $
      *
      *   Rev 2.7   Aug 10 2017 16:17:22   t13ae13
      *ASL00567: LUCY2 für Ausbilderbonus - SL12 (swl)
      *
      *   Rev 2.6   Feb 17 2016 09:32:02   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.5   Feb 17 2016 09:16:34   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.4   Feb 17 2016 08:46:58   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.3   Feb 17 2016 07:50:22   t13ae14
      *ASL00530: SL12 - Dialog Ausbilder-Zuschuss
      *
      *   Rev 2.2   Dec 09 2015 14:03:12   t13ex13
      *ASL00530: RGT: SL12, Map
      *
      *   Rev 2.1   Apr 07 2015 11:28:04   T13EX13
      *ASL00530: RGT: SL12, Map
      *
      *   Rev 2.0   Mar 31 2015 13:41:26   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BSL0014
      *   Date Created  07/13/2017
      *   Time Created  09:08:52

      *  Input Data For Map BSL0014
         01 BSL0014I.
            03 FILLER                         PIC X(12).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).
            03 EVBNRL                         PIC S9(4) COMP.
            03 EVBNRF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRI                         PIC X(7).
            03 EPRZL                          PIC S9(4) COMP.
            03 EPRZF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EPRZI                          PIC X(1).
            03 AEQFEIGL                       PIC S9(4) COMP.
            03 AEQFEIGF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AEQFEIGI                       PIC X(5).
            03 AMINEHL                        PIC S9(4) COMP.
            03 AMINEHF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AMINEHI                        PIC X(5).
            03 EMONATML                       PIC S9(4) COMP.
            03 EMONATMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONATMI                       PIC X(2).
            03 EMONATJL                       PIC S9(4) COMP.
            03 EMONATJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EMONATJI                       PIC X(2).
            03 EKZERFL                        PIC S9(4) COMP.
            03 EKZERFF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKZERFI                        PIC X(4).
            03 EVERGEHL                       PIC S9(4) COMP.
            03 EVERGEHF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EVERGEHI                       PIC X(7).
            03 DFHG001I                       OCCURS 10 TIMES.
               05 ABETREUL                       PIC S9(4) COMP.
               05 ABETREUF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABETREUI                       PIC X(7).
               05 AEBENEL                        PIC S9(4) COMP.
               05 AEBENEF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AEBENEI                        PIC X(1).
               05 ADATUML                        PIC S9(4) COMP.
               05 ADATUMF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ADATUMI                        PIC X(6).
               05 EBETRAGL                       PIC S9(4) COMP.
               05 EBETRAGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EBETRAGI                       PIC X(8).
               05 EBETRUPL                       PIC S9(4) COMP.
               05 EBETRUPF                       PIC X.
               05 FILLER                         PIC X(1).
               05 EBETRUPI                       PIC X(8).

      *  Output Data For Map BSL0014
         01 BSL0014O REDEFINES BSL0014I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).
            03 FILLER                         PIC X(2).
            03 EVBNRA                         PIC X.
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
            03 FILLER                         PIC X(2).
            03 EPRZA                          PIC X.
            03 EPRZH                          PIC X.
            03 EPRZO                          PIC X(1).
            03 FILLER                         PIC X(2).
            03 AEQFEIGA                       PIC X.
            03 AEQFEIGH                       PIC X.
            03 AEQFEIGO                       PIC X(5).
            03 FILLER                         PIC X(2).
            03 AMINEHA                        PIC X.
            03 AMINEHH                        PIC X.
            03 AMINEHO                        PIC X(5).
            03 FILLER                         PIC X(2).
            03 EMONATMA                       PIC X.
            03 EMONATMH                       PIC X.
            03 EMONATMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EMONATJA                       PIC X.
            03 EMONATJH                       PIC X.
            03 EMONATJO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EKZERFA                        PIC X.
            03 EKZERFH                        PIC X.
            03 EKZERFO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 EVERGEHA                       PIC X.
            03 EVERGEHH                       PIC X.
            03 EVERGEHO                       PIC X(7).
            03 DFHG001O OCCURS 10.
               05 FILLER                         PIC X(2).
               05 ABETREUA                       PIC X.
               05 ABETREUH                       PIC X.
               05 ABETREUO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 AEBENEA                        PIC X.
               05 AEBENEH                        PIC X.
               05 AEBENEO                        PIC X(1).
               05 FILLER                         PIC X(2).
               05 ADATUMA                        PIC X.
               05 ADATUMH                        PIC X.
               05 ADATUMO                        PIC X(6).
               05 FILLER                         PIC X(2).
               05 EBETRAGA                       PIC X.
               05 EBETRAGH                       PIC X.
               05 EBETRAGO                       PIC X(8).
               05 FILLER                         PIC X(2).
               05 EBETRUPA                       PIC X.
               05 EBETRUPH                       PIC X.
               05 EBETRUPO                       PIC X(8).


